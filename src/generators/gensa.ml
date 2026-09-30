(*
	The Haxe Compiler
	Copyright (C) 2005-2026  Haxe Foundation

	This program is free software; you can redistribute it and/or
	modify it under the terms of the GNU General Public License
	as published by the Free Software Foundation; either version 2
	of the License, or (at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program; if not, write to the Free Software
	Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
*)

(*
	SA target (Safe ASM, 安全汇编).

	Emits SA text assembly (`.sa`) that feeds the `sci` toolchain
	(`sa build-exe` / `sa build-wasm`). All runtime calls lower to the
	canonical `sci/sa_std` contracts only (see `SA_TARGET.md`); this
	generator never invents new std APIs.

	Shape reference: `sala` help docs `content/03_sa_asm/02_sa_syntax.html`
	(minimal program, flat labels, explicit `!` releases) and
	`sci/demos/rosetta/01_hello_world/main.sa`.

	Ownership model (matches SLA "stack homing", see sala
	`content/02_sla_lang/06_ownership.html`): every Haxe `let` is homed
	to a `stack_alloc 8` slot; value uses auto-`load`, `&x` is the slot
	address. All live regs are released (`!r`) on every exit.

	v0.2 status: straight-line `@main` lowering (int consts, let homing,
	int arithmetic/comparisons, bool logic, `trace` of string literals).
	Control flow (`if`/`while`/`switch`) is v0.3 and currently emits an
	honest `// SA-TODO(v0.3)` comment.
*)

open Globals
open Ast
open Type
open Gctx

(** An SA operand: immediate literal or materialized register. *)
type operand =
	| Imm of string
	| Reg of string

type ctx = {
	com : Gctx.t;
	buf : Buffer.t;
	header : Buffer.t;
	mutable next_reg : int;
	mutable next_str : int;
	mutable next_label : int;
	vars : (int, string) Hashtbl.t;
	mutable live : string list;
	(** Emitted helper functions (SA name -> unit), pre-registered so
		recursion and forward calls resolve during lowering. *)
	emitted : (string, unit) Hashtbl.t;
	(** Out-of-line helper bodies, spliced after @main. *)
	funcs : Buffer.t;
	(** Loop stack: (end_label, cond_label, entry_snap, cond_snap).
		`entry_snap` is the live length at loop entry (plain vars);
		`cond_snap` the length after the condition is evaluated
		(cond temps included). All edges into L_COND carry exactly
		`entry_snap` regs; all edges into L_END carry `cond_snap` regs,
		so every merge is Phi-consistent (see sala 06_limitations). *)
	mutable loops : (string * string * int * int ref) list;
	(** String lengths: slots hold only the pointer, so lengths of
		string-typed locals are tracked here (v_id -> Imm length or
		Reg holding it). Unknown = absent. *)
	str_lens : (int, operand) Hashtbl.t;
	(** Scratch stack slot for buffered print data (created at entry,
		so no branch-local alloc). Passed as `&pslot` to satisfy the
		borrow contract of `sa_print_bytes`. *)
	mutable pslot : string option;
	(** Homed `this` slot for methods/constructors (None outside). *)
	mutable this_slot : string option;
	(** Enclosing function return kind ("i32", "void", "String", ...).
		Drives String-return pairing. *)
	mutable ret_kind : string;
}

let fresh ctx prefix =
	let id = ctx.next_reg in
	ctx.next_reg <- id + 1;
	Printf.sprintf "%s%d" prefix id

let fresh_label ctx prefix =
	let id = ctx.next_label in
	ctx.next_label <- id + 1;
	Printf.sprintf "L_%s%d" prefix id

let emit ctx s =
	Buffer.add_string ctx.buf "    ";
	Buffer.add_string ctx.buf s;
	Buffer.add_char ctx.buf '\n'

let comment ctx s =
	Buffer.add_string ctx.buf "    // ";
	Buffer.add_string ctx.buf s;
	Buffer.add_char ctx.buf '\n'

(** Labels start at column 0 (macro/label rule, see sala 06_limitations). *)
let emit_label ctx l =
	Buffer.add_string ctx.buf l;
	Buffer.add_string ctx.buf ":\n"

let track ctx r =
	ctx.live <- r :: ctx.live

(** Forget a register without emitting (paired with an explicit release
	emitted by the caller). *)
let forget ctx r =
	ctx.live <- List.filter (fun x -> x <> r) ctx.live

(** Immediate post-use cleanup: emit `!r` and drop bookkeeping. *)
let release_now ctx r =
	emit ctx ("!" ^ r);
	forget ctx r

(** Unwrap an operand to source text. *)
let ops = function Imm s -> s | Reg r -> r

(** Path-discipline helpers: sibling branch paths are generated
	sequentially but execute exclusively. Each arm must start from the
	branch-entry list state, or releases emitted for one path corrupt
	the bookkeeping of later siblings (Referee UnknownRegister /
	PhiStateConflict). *)
let save_live ctx = ctx.live

let restore_live ctx s = ctx.live <- s

let rec take_live n lst =
	if n <= 0 then [] else match lst with
	| [] -> []
	| x :: xs -> x :: take_live (n - 1) xs

(** Keep the OLDEST n entries (the merge survivors); drop newer temps.
	Used when restoring bookkeeping after a multi-path construct. *)
let keep_oldest n lst =
	List.rev (take_live n (List.rev lst))

let is_float_t t =
	match follow t with
	| TAbstract ({ a_path = ([], "Float") }, _) -> true
	| TInst ({ cl_path = ([], "Float") }, _) -> true
	| _ -> false

(** True when the (followed) type is Haxe Int (needs sitofp in a
	float context). *)
let is_int_t t =
	match follow t with
	| TAbstract ({ a_path = ([], "Int") }, _) -> true
	| _ -> false

(** Memory annotation for homed slots: Float -> f64, Int/Bool -> i32,
	UInt -> u64, everything else (Array/String/objects) -> ptr.
	Deterministic per type so matching load/store pairs always agree. *)
let mem_ty t =
	if is_float_t t then "f64"
	else match follow t with
	| TAbstract ({ a_path = ([], "Int") }, _)
	| TAbstract ({ a_path = ([], "Bool") }, _) -> "i32"
	| TAbstract ({ a_path = ([], "UInt") }, _) -> "u64"
	| _ -> "ptr"

(** Integer arithmetic mnemonic (None = unsupported for v0.9 ints). *)
let arith_mnemonic op =
	match op with
	| OpAdd -> Some "add" | OpSub -> Some "sub" | OpMult -> Some "mul"
	| OpDiv -> Some "sdiv" | OpMod -> Some "srem"
	| OpAnd -> Some "and" | OpOr -> Some "or" | OpXor -> Some "xor"
	| OpShl -> Some "shl" | OpShr -> Some "ashr" | OpUShr -> Some "lshr"
	| _ -> None

(** Simple bases safe to evaluate twice (no side effects). *)
let rec is_simple_base e =
	match e.eexpr with
	| TLocal _ -> true
	| TConst TThis -> true
	| TParenthesis e1 | TMeta (_, e1) | TCast (e1, _) -> is_simple_base e1
	| _ -> false

let sa_escape s =
	let b = Buffer.create (String.length s) in
	String.iter (fun c ->
		match c with
		| '"' -> Buffer.add_string b "\\\""
		| '\\' -> Buffer.add_string b "\\\\"
		| '\n' -> Buffer.add_string b "\\n"
		| '\t' -> Buffer.add_string b "\\t"
		| '\r' -> Buffer.add_string b "\\r"
		| c -> Buffer.add_char b c
	) s;
	Buffer.contents b

let intern_string ctx s =
	let id = ctx.next_str in
	ctx.next_str <- id + 1;
	let name = Printf.sprintf "STR_%d" id in
	Buffer.add_string ctx.header
		(Printf.sprintf "@const %s = utf8:\"%s\"\n" name (sa_escape s));
	(name, String.length s)

(** True when the (followed) type is Haxe String. *)
let is_string_t t =
	match follow t with
	| TInst ({ cl_path = ([], "String") }, _) -> true
	| _ -> false

(** Resolve a string expression to (ptr_src, len_src) source strings.
	Literals intern inline; locals need a tracked length. *)
let rec string_operands ctx e =
	match e.eexpr with
	| TConst (TString s) ->
		let (name, len) = intern_string ctx s in
		Some ("&" ^ name, string_of_int len)
	| TLocal v when is_string_t v.v_type -> begin
		try
			let slot = Hashtbl.find ctx.vars v.v_id in
			let b = fresh ctx "t" in
			emit ctx (Printf.sprintf "%s = load %s+0 as ptr" b slot);
			track ctx b;
			let ls = match Hashtbl.find ctx.str_lens v.v_id with
				| Imm s -> s | Reg r -> r in
			Some (b, ls)
		with Not_found -> None
	end
	| TParenthesis e1 | TMeta (_, e1) | TCast (e1, _) -> string_operands ctx e1
	| _ -> None

(** Record a string local's length from its initializer. *)
let track_str_len ctx v init_opt =
	if is_string_t v.v_type then begin
		let len_opt = match init_opt with
			| Some { eexpr = TConst (TString s) } -> Some (Imm (string_of_int (String.length s)))
			| Some { eexpr = TLocal w } ->
				(try Some (Hashtbl.find ctx.str_lens w.v_id) with Not_found -> None)
			| _ -> None in
		match len_opt with
		| Some l -> Hashtbl.replace ctx.str_lens v.v_id l
		| None ->
			Hashtbl.remove ctx.str_lens v.v_id
	end

let expr_kind e =
	match e.eexpr with
	| TConst _ -> "TConst" | TLocal _ -> "TLocal" | TArray _ -> "TArray"
	| TBinop (op, _, _) -> "TBinop:" ^ Ast.s_binop op
	| TField _ -> "TField" | TTypeExpr _ -> "TTypeExpr"
	| TParenthesis _ -> "TParenthesis" | TObjectDecl _ -> "TObjectDecl"
	| TArrayDecl _ -> "TArrayDecl" | TCall _ -> "TCall" | TNew _ -> "TNew"
	| TUnop _ -> "TUnop" | TFunction _ -> "TFunction" | TVar _ -> "TVar"
	| TBlock _ -> "TBlock" | TIf _ -> "TIf"
	| TWhile _ -> "TWhile" | TSwitch _ -> "TSwitch" | TTry _ -> "TTry"
	| TReturn _ -> "TReturn" | TBreak -> "TBreak" | TContinue -> "TContinue"
	| TThrow _ -> "TThrow" | TCast _ -> "TCast" | TMeta _ -> "TMeta"
	| TEnumParameter _ -> "TEnumParameter" | TEnumIndex _ -> "TEnumIndex"
	| TIdent _ -> "TIdent"

let sa_fun_name c cf =
	"hx_" ^ String.concat "_" (fst c.cl_path @ [snd c.cl_path]) ^ "_" ^ cf.cf_name

(** SA name for a Haxe constructor: `hx_<flat path>_new`. *)
let sa_ctor_name c =
	"hx_" ^ String.concat "_" (fst c.cl_path @ [snd c.cl_path]) ^ "_new"

(** Scalar return annotation (None = not a plain scalar signature).
	Enums pass as opaque object pointers. *)
let scalar_ret t =
	match follow t with
	| TAbstract ({ a_path = ([], "Int") }, _)
	| TAbstract ({ a_path = ([], "Bool") }, _) -> Some "i32"
	| TAbstract ({ a_path = ([], "Float") }, _) -> Some "f64"
	| TAbstract ({ a_path = ([], "Void") }, _) -> Some "void"
	| TEnum _ -> Some "ptr"
	| _ -> None

let scalar_param t = match scalar_ret t with
	| Some "void" -> None
	| x -> x

(** True when the (followed) type is a Haxe enum (passed as an
	opaque object pointer, like arrays). *)
let is_enum_t t =
	match follow t with
	| TEnum _ -> true
	| _ -> false

(** Expanded parameter: scalars pass by value, strings as
	`(name_ptr: ptr, name_len: u64)` borrow pairs. *)type param_exp =
	| Scalar of tvar * string
	| StrPair of tvar

let str_ptr_name v = v.v_name ^ "_ptr"
let str_len_name v = v.v_name ^ "_len"

(** Expand tf_args against the TFun formal types. None = some param
	is neither scalar nor String (function skipped honestly). *)
(** True when a type mentions a type parameter (bare generic). Used
	to give actionable guidance instead of a cryptic TODO. *)
let rec mentions_param t =
	match t with
	| TInst ({ cl_kind = KTypeParameter _ }, _) -> true
	| TInst (_, tl) | TAbstract (_, tl) | TEnum (_, tl) ->
		List.exists mentions_param tl
	| TFun (args, ret) ->
		List.exists (fun (_, _, a) -> mentions_param a) args
		|| mentions_param ret
	| TType (_, tl) -> List.exists mentions_param tl
	| TAnon a ->
		PMap.fold (fun cf acc -> acc || mentions_param cf.cf_type) a.a_fields false
	| TDynamic (Some t2) -> mentions_param t2
	| _ -> false

(** Guidance for bare-generic rejections (v0.22 boundary). *)
let generic_hint ctx c cf =
	comment ctx ("SA-NOTE(v0.22): bare generic " ^
		s_type_path c.cl_path ^ "." ^ cf.cf_name ^
		" skipped; add @:generic for monomorphization")

(** True when a class field signature mentions type parameters. *)
let cf_is_bare_generic cf =
	mentions_param cf.cf_type

let expand_sig tf args ret_of =
	let formals = List.map (fun (_, _, t) -> t) args in
	let paired =
		try List.combine (List.map fst tf.tf_args) formals
		with Invalid_argument _ -> [] in
	if List.length paired <> List.length tf.tf_args then None
	else begin
		let exps = List.map (fun (v, t) ->
			if is_string_t t then Some (StrPair v)
			else match scalar_param t with
				| Some ty -> Some (Scalar (v, ty))
				| None -> None
		) paired in
		if List.exists ((=) None) exps then None
		else
			let exps = List.map (function Some x -> x | None -> assert false) exps in
			match ret_of with
			| None -> Some (tf, exps, "")
			| Some rt ->
				if is_string_t rt then Some (tf, exps, "String")
				else (match scalar_ret rt with
				| None -> None
				| Some rs -> Some (tf, exps, rs))
	end

(** A static helper is emittable when it is a plain method with a body;
	params may be scalars or Strings (borrow pairs); other aggregates
	and non-scalar returns skip it honestly. *)
let emittable_static cf =
	match cf.cf_kind with
	| Method MethNormal | Method MethInline -> begin
		match cf.cf_expr with
		| Some { eexpr = TFunction tf } -> begin
			match follow cf.cf_type with
			| TFun (args, ret) -> expand_sig tf args (Some ret)
			| _ -> None
		end
		| _ -> None
	end
	| _ -> None

(** Instance Var fields in declaration order with 8-byte offsets. *)
let class_layout c =
	let off = ref 0 in
	List.filter_map (fun cf ->
		match cf.cf_kind with
		| Var _ ->
			let o = !off in
			off := o + 8;
			Some (cf, o)
		| _ -> None
	) c.cl_ordered_fields

let class_size c = List.length (class_layout c) * 8

(** Field byte offset in the class layout, if an instance Var. *)
let field_offset c name =
	let rec loop = function
		| [] -> None
		| (cf, o) :: rest ->
			if cf.cf_name = name then Some o else loop rest in
	loop (class_layout c)

(** True when the body can abort via SA `panic` (explicit `throw`
	or I/O-trace failure paths). Such `try` blocks keep an honest
	TODO; panic-free bodies lower straight through with the handler
	elided as unreachable (no abort source exists in our subset). *)
let rec try_may_abort e =
	match e.eexpr with
	| TThrow _ -> true
	| TCall ({ eexpr = TField (_, FStatic (c, cf)) }, args) ->
		let abort_call =
			(s_type_path c.cl_path = "sys.io.File" && cf.cf_name = "getContent")
			|| (s_type_path c.cl_path = "Sys" && cf.cf_name = "getEnv") in
		abort_call || List.exists try_may_abort args
	| TBlock el -> List.exists try_may_abort el
	| TVar (_, init) ->
		(match init with Some x -> try_may_abort x | None -> false)
	| TBinop (_, a, b) -> try_may_abort a || try_may_abort b
	| TUnop (_, _, x) -> try_may_abort x
	| TIf (c, t, eo) -> try_may_abort c || try_may_abort t
		|| (match eo with Some x -> try_may_abort x | None -> false)
	| TWhile (c, b, _) -> try_may_abort c || try_may_abort b
	| TSwitch sw ->
		try_may_abort sw.switch_subject
		|| List.exists (fun cs ->
			List.exists try_may_abort cs.case_patterns
			|| try_may_abort cs.case_expr) sw.switch_cases
		|| (match sw.switch_default with Some x -> try_may_abort x | None -> false)
	| TTry (e1, catches) -> try_may_abort e1
		|| List.exists (fun (_, e2) -> try_may_abort e2) catches
	| TArray (a, i) -> try_may_abort a || try_may_abort i
	| TArrayDecl el -> List.exists try_may_abort el
	| TObjectDecl fl -> List.exists (fun (_, x) -> try_may_abort x) fl
	| TField (o, _) -> try_may_abort o
	| TCall (f, args) -> try_may_abort f || List.exists try_may_abort args
	| TNew (_, _, args) -> List.exists try_may_abort args
	| TReturn r ->
		(match r with Some x -> try_may_abort x | None -> false)
	| TParenthesis e1 | TMeta (_, e1) | TCast (e1, _)
	| TEnumParameter (e1, _, _) | TEnumIndex e1 -> try_may_abort e1
	| _ -> false
let rec has_return e =
	match e.eexpr with
	| TReturn _ -> true
	| TBlock el -> List.exists has_return el
	| TIf (c, t, eo) -> has_return c || has_return t
		|| (match eo with Some x -> has_return x | None -> false)
	| TWhile (c, b, _) -> has_return c || has_return b
	| TSwitch sw ->
		has_return sw.switch_subject
		|| List.exists (fun cs ->
			List.exists has_return cs.case_patterns
			|| has_return cs.case_expr) sw.switch_cases
		|| (match sw.switch_default with Some x -> has_return x | None -> false)
	| TTry (e1, catches) -> has_return e1
		|| List.exists (fun (_, e2) -> has_return e2) catches
	| TParenthesis e1 | TMeta (_, e1) | TCast (e1, _) -> has_return e1
	| _ -> false

(** An emittable constructor: plain method named `new`, scalar/String
	params, body without explicit returns. *)
let emittable_ctor c cf =
	if cf.cf_name <> "new" then None
	else match cf.cf_kind with
	| Method MethNormal | Method MethInline -> begin
		match cf.cf_expr with
		| Some { eexpr = TFunction tf } -> begin
			match follow cf.cf_type with
			| TFun (args, _) -> begin
				match expand_sig tf args None with
				| None -> None
				| Some (tf, exps, _) ->
					if has_return tf.tf_expr then None
					else Some (tf, exps)
			end
			| _ -> None
		end
		| _ -> None
	end
	| _ -> None

(** An emittable instance method: like statics, plus implicit self. *)
let emittable_method cf =
	match cf.cf_kind with
	| Method MethNormal | Method MethInline -> begin
		match cf.cf_expr with
		| Some { eexpr = TFunction tf } -> begin
			match follow cf.cf_type with
			| TFun (args, ret) -> expand_sig tf args (Some ret)
			| _ -> None
		end
		| _ -> None
	end
	| _ -> None

(** Reachability walk over main + helper bodies: which classes need
	ctors, methods, statics. Constructors imply field layouts. *)
let rec collect_classes acc e =
	let add_ctor acc c =
		if List.exists (fun (k, _) -> k = "ctor:" ^ s_type_path c.cl_path) acc
		then acc
		else ("ctor:" ^ s_type_path c.cl_path, `Ctor c) :: acc in
	let add_method acc c cf =
		let k = "method:" ^ s_type_path c.cl_path ^ "." ^ cf.cf_name in
		if List.exists (fun (kk, _) -> kk = k) acc then acc
		else (k, `Method (c, cf)) :: acc in
	let add_static acc c cf =
		let k = "static:" ^ s_type_path c.cl_path ^ "." ^ cf.cf_name in
		if List.exists (fun (kk, _) -> kk = k) acc then acc
		else (k, `Static (c, cf)) :: acc in
	let rec walk acc e = match e.eexpr with
		| TNew (c, _, args) ->
			List.fold_left walk (add_ctor acc c) args
		| TCall ({ eexpr = TField (obj, FInstance (c, _, cf)) }, args) ->
			List.fold_left walk
				(List.fold_left walk (add_method acc c cf) args) [obj]
		| TCall ({ eexpr = TField (_, FStatic (c, cf)) }, args) ->
			List.fold_left walk (add_static acc c cf) args
		| TField (obj, FInstance (c, _, cf)) ->
			walk (add_method acc c cf) obj
		| TBlock el -> List.fold_left walk acc el
		| TVar (_, init) ->
			(match init with Some x -> walk acc x | None -> acc)
		| TBinop (_, a, b) -> walk (walk acc a) b
		| TUnop (_, _, x) -> walk acc x
		| TIf (c, t, eo) ->
			let acc = walk (walk acc c) t in
			(match eo with Some x -> walk acc x | None -> acc)
		| TWhile (c, b, _) -> walk (walk acc c) b
		| TSwitch sw ->
			let acc = walk acc sw.switch_subject in
			let acc = List.fold_left (fun a cs ->
				List.fold_left walk a (cs.case_expr :: cs.case_patterns)
			) acc sw.switch_cases in
			(match sw.switch_default with Some x -> walk acc x | None -> acc)
		| TTry (e1, catches) ->
			List.fold_left (fun a (_, e2) -> walk a e2) (walk acc e1) catches
		| TArray (a, i) -> walk (walk acc a) i
		| TArrayDecl el -> List.fold_left walk acc el
		| TObjectDecl fl -> List.fold_left walk acc (List.map snd fl)
		| TCall (f, args) -> List.fold_left walk (walk acc f) args
		| TReturn r ->
			(match r with Some x -> walk acc x | None -> acc)
		| TThrow x -> walk acc x
		| TField (obj, _) -> walk acc obj
		| TCast (x, _) | TParenthesis x | TMeta (_, x)
		| TEnumParameter (x, _, _) | TEnumIndex x -> walk acc x
		| _ -> acc in
	walk acc e

let new_fun_ctx com header emitted funcs = {
	com; buf = Buffer.create 2048; header;
	next_reg = 0; next_str = 0; next_label = 0;
	vars = Hashtbl.create 16; live = []; loops = [];
	emitted; funcs;
	str_lens = Hashtbl.create 8; pslot = None; this_slot = None; ret_kind = "i32";
}

(** Lazily create the entry scratch slot for buffered prints. Must be
	called at function entry (before any branch). *)
let ensure_pslot ctx =
	match ctx.pslot with
	| Some s -> s
	| None ->
		let s = fresh ctx "pslot" in
		emit ctx (Printf.sprintf "%s = stack_alloc 8" s);
		ctx.pslot <- Some s;
		s

(** Null test (a `null` side of `==` compares pointers, correctly). *)
let rec is_null_expr e =
	match e.eexpr with
	| TConst TNull -> true
	| TParenthesis e1 | TMeta (_, e1) | TCast (e1, _) -> is_null_expr e1
	| _ -> false

(** String `==` / `!=` via the supplemented `STRING_EQ`/`STRING_NEQ`
	macros (pointer comparison would be wrong for content). A `null`
	side falls through to plain pointer comparison. *)
(** Resolve the entry body: `main_expr` is usually a `TCall` to the
	static `main` method (or a `TBlock` ending in the `EntryPoint.run`
	call). Returns the statements, whether the entry takes arguments,
	and the main class (whose statics become SA helpers). *)

let callee_kind e =
	match e.eexpr with
	| TIdent s -> "Ident:" ^ s
	| TField (_, FStatic (c, f)) -> "FStatic:" ^ s_type_path c.cl_path ^ "." ^ f.cf_name
	| TField (_, FInstance (c, _, f)) -> "FInstance:" ^ s_type_path c.cl_path ^ "." ^ f.cf_name
	| TField _ -> "FOther"
	| TLocal v -> "Local:" ^ v.v_name
	| TConst _ -> "Const"
	| _ -> "Other"

let gen_trace_lit ctx args =
	match args with
	| { eexpr = TConst (TString s) } :: _ ->
		let (name, len) = intern_string ctx s in
		emit ctx (Printf.sprintf "call @sa_print_bytes(&%s, %d)" name len);
		true
	| _ -> false


let release_all_except ctx keep =
	List.iter (fun r ->
		match keep with
		| Some k when k = r -> ()
		| _ -> emit ctx ("!" ^ r)
	) (List.rev ctx.live);
	ctx.live <- (match keep with Some k -> [k] | None -> [])

(** Arm-local cleanup (Phi consistency, see sala 06_limitations):
	release registers created after the snapshot so that every edge
	arriving at a merge label carries the same live set. *)
let snapshot ctx = List.length ctx.live

let release_since ctx n =
	let rec split i acc rest =
		if i <= 0 then (List.rev acc, rest)
		else match rest with
			| [] -> (List.rev acc, [])
			| r :: rs -> split (i - 1) (r :: acc) rs
	in
	let (fresh_regs, outer) = split (List.length ctx.live - n) [] ctx.live in
	List.iter (fun r -> emit ctx ("!" ^ r)) (List.rev fresh_regs);
	ctx.live <- outer

(** Pre-pass: collect every `let` in the entry statements (including
	inside `if`/`while` bodies) so `stack_alloc`s can be hoisted above
	all branches. Branch-local `stack_alloc` would trap with
	`PhiStateConflict` (see sala 06_limitations). *)

(** Field byte offset in the class layout, if an instance Var. *)
let field_offset c name =
	let rec loop = function
		| [] -> None
		| (cf, o) :: rest ->
			if cf.cf_name = name then Some o else loop rest in
	loop (class_layout c)

(** Lower `obj.field` reads (instance Var fields only). *)
(** v0.11 sys surface: `sys.io.File`, `Sys`, `sys.FileSystem` lowered
	to existing `sci/sa_std` fs/env contracts only (macros + externs,
	zero new ABI). Statement/trace-oriented like v0.6b; stored
	computed results and sockets stay TODOs. *)
let sys_save_content ctx p content =
	match string_operands ctx p, string_operands ctx content with
	| Some (pp, pl), Some (bp, bl) ->
		let st = fresh ctx "t" in
		emit ctx (Printf.sprintf "EXPAND FS_WRITE_FILE %s, %s, %s, %s, %s"
			st pp pl bp bl);
		track ctx st;
		release_now ctx st;
		true
	| _ ->
		comment ctx "SA-TODO(v0.11): saveContent needs resolvable strings";
		false

let sys_surface_stmt ctx c cf args : bool =
	match s_type_path c.cl_path, cf.cf_name, args with
	| "sys.io.File", "saveContent", [_; _] ->
		(match args with
		| [p; content] -> sys_save_content ctx p content
		| _ -> false)
	| _ -> false

(** `sys.FileSystem.exists(path)` as a plain i32 operand.
	`&path` borrow goes through the entry scratch slot. *)
let sys_exists_op ctx path =
	match string_operands ctx path with
	| Some (pp, pl) ->
		let ps = ensure_pslot ctx in
		emit ctx (Printf.sprintf "store %s+0, %s as ptr" ps pp);
		let r = fresh ctx "t" in
		emit ctx (Printf.sprintf "%s = call @sa_std_fs_exists(&%s, %s)" r ps pl);
		track ctx r;
		Some (Reg r)
	| None ->
		comment ctx "SA-TODO(v0.11): exists needs resolvable path";
		None

let sys_surface_operand ctx c cf args =
	match s_type_path c.cl_path, cf.cf_name, args with
	| "sys.io.File", "saveContent", [p; content] ->
		if sys_save_content ctx p content then Some (Imm "0") else None
	| "sys.FileSystem", "exists", [path] -> sys_exists_op ctx path
	| _ -> None

(** Trace `sys.io.File.getContent(path)` directly: read handle,
	print, free; I/O failure panics loudly (no silent garbage). *)
let trace_fs_get_content ctx p =
	match string_operands ctx p with
	| Some (pp, pl) ->
		let st = fresh ctx "t" in
		let buf = fresh ctx "t" in
		emit ctx (Printf.sprintf
			"EXPAND FS_READ_TO_STRING %s, %s, %s, %s, 1048576" st buf pp pl);
		track ctx st;
		track ctx buf;
		let ok = fresh ctx "t" in
		emit ctx (Printf.sprintf "%s = eq %s, 0" ok st);
		track ctx ok;
		let l_ok = fresh_label ctx "FSOK" in
		let l_fail = fresh_label ctx "FSFAIL" in
		let l_end = fresh_label ctx "FSEND" in
		emit ctx (Printf.sprintf "br %s -> %s, %s" ok l_ok l_fail);
		emit_label ctx l_ok;
		let ps = ensure_pslot ctx in
		let d = fresh ctx "t" in
		emit ctx (Printf.sprintf "%s = call @sa_fs_read_buffer_data(%s)" d buf);
		track ctx d;
		let ln = fresh ctx "t" in
		emit ctx (Printf.sprintf "%s = call @sa_fs_read_buffer_len(%s)" ln buf);
		track ctx ln;
		emit ctx (Printf.sprintf "store %s+0, %s as ptr" ps d);
		emit ctx (Printf.sprintf "call @sa_print_bytes(&%s, %s)" ps ln);
		let f = fresh ctx "t" in
		emit ctx (Printf.sprintf "%s = call @sa_fs_read_buffer_free(^%s)" f buf);
		track ctx f;
		forget ctx buf;
		release_now ctx st;
		release_now ctx ok;
		release_now ctx d;
		release_now ctx ln;
		release_now ctx f;
		emit ctx (Printf.sprintf "jmp %s" l_end);
		emit_label ctx l_fail;
		emit ctx "panic(\"haxe:fs-read\")";
		emit_label ctx l_end;
		true
	| None -> false

(** Trace a registry-handle string (env/cwd): miss check, print,
	free. Shared by getEnv/getCwd trace paths. *)
let trace_handle_string ctx h data_fn len_fn free_fn miss_msg =
	let is0 = fresh ctx "t" in
	emit ctx (Printf.sprintf "%s = eq %s, 0" is0 h);
	track ctx is0;
	let l_miss = fresh_label ctx "HMISS" in
	let l_hit = fresh_label ctx "HHIT" in
	let l_end = fresh_label ctx "HEND" in
	emit ctx (Printf.sprintf "br %s -> %s, %s" is0 l_miss l_hit);
	emit_label ctx l_miss;
	emit ctx (Printf.sprintf "panic(\"%s\")" miss_msg);
	emit_label ctx l_hit;
	let ps = (match ctx.pslot with Some s -> s | None -> ensure_pslot ctx) in
	let d = fresh ctx "t" in
	emit ctx (Printf.sprintf "%s = call @%s(%s)" d data_fn h);
	track ctx d;
	let ln = fresh ctx "t" in
	emit ctx (Printf.sprintf "%s = call @%s(%s)" ln len_fn h);
	track ctx ln;
	emit ctx (Printf.sprintf "store %s+0, %s as ptr" ps d);
	emit ctx (Printf.sprintf "call @sa_print_bytes(&%s, %s)" ps ln);
	let f = fresh ctx "t" in
	emit ctx (Printf.sprintf "%s = call @%s(^%s)" f free_fn h);
	track ctx f;
	forget ctx h;
	release_now ctx is0;
	release_now ctx d;
	release_now ctx ln;
	release_now ctx f;
	emit ctx (Printf.sprintf "jmp %s" l_end);
	emit_label ctx l_end;
	true

(** Trace `Sys.getEnv(name)` directly; missing variable panics. *)
let trace_sys_getenv ctx n =
	match string_operands ctx n with
	| Some (kp, kl) ->
		let h = fresh ctx "h" in
		emit ctx (Printf.sprintf "%s = call @sa_env_get(%s, %s)" h kp kl);
		track ctx h;
		trace_handle_string ctx h
			"sa_env_buffer_data" "sa_env_buffer_len" "sa_env_buffer_free"
			"haxe:env-missing"
	| None -> false

(** Trace `Sys.getCwd()` directly. *)
let trace_sys_getcwd ctx =
	let h = fresh ctx "h" in
	emit ctx (Printf.sprintf "%s = call @sa_env_current_dir()" h);
	track ctx h;
	trace_handle_string ctx h
		"sa_env_buffer_data" "sa_env_buffer_len" "sa_env_buffer_free"
		"haxe:cwd-missing"

(** `Sys.putEnv(k, v)` statement. Status ignored like saveContent. *)
let sys_putenv ctx k v =
	match string_operands ctx k, string_operands ctx v with
	| Some (kp, kl), Some (vp, vl) ->
		let st = fresh ctx "t" in
		emit ctx (Printf.sprintf "%s = call @sa_env_set_var(%s, %s, %s, %s)"
			st kp kl vp vl);
		track ctx st;
		release_now ctx st;
		true
	| _ ->
		comment ctx "SA-TODO(v0.17): putEnv needs resolvable strings";
		false

(** `Sys.time()` operand: unix_ms to Float seconds. *)
let sys_time_op ctx =
	let m = fresh ctx "t" in
	emit ctx (Printf.sprintf "%s = call @sa_time_unix_ms()" m);
	track ctx m;
	let f = fresh ctx "t" in
	emit ctx (Printf.sprintf "%s = sitofp %s" f m);
	track ctx f;
	let s = fresh ctx "t" in
	emit ctx (Printf.sprintf "%s = fdiv %s, 1000.0" s f);
	track ctx s;
	Reg s
let rec gen_operand ctx e =
	match e.eexpr with
	| TConst (TInt i) -> Imm (Int32.to_string i)
	| TConst (TFloat f) -> Imm f
	| TConst (TBool b) -> Imm (if b then "1" else "0")
	| TConst TNull -> Imm "0"
	| TConst (TString s) ->
		(* String value: address of the interned constant. Only the
			address fits an 8-byte slot; length recovery needs the fmt
			buffer (v0.5). Direct literal trace avoids this path. *)
		let (name, _) = intern_string ctx s in
		Imm ("&" ^ name)
	| TConst TThis -> begin
		match ctx.this_slot with
		| None ->
			comment ctx "SA-TODO(v0.9): this outside method";
			Imm "0"
		| Some slot ->
			let r = fresh ctx "t" in
			emit ctx (Printf.sprintf "%s = load %s+0 as ptr" r slot);
			track ctx r;
			Reg r
	end
	| TNew (c, _, args) -> gen_new ctx c args
	| TCall ({ eexpr = TField (_, FStatic (c, cf)) }, [])
		when s_type_path c.cl_path = "Date" && cf.cf_name = "now" ->
		date_now_op ctx
	| TCall ({ eexpr = TField (obj, FInstance (c, _, cf)) }, _)
		when s_type_path c.cl_path = "Date" ->
		date_method_op ctx obj cf
	| TCall ({ eexpr = TField (obj, FInstance (c, _, cf)) }, args) ->
		gen_method_call ctx c cf obj args
	| TCall ({ eexpr = TField (_, FStatic (c, cf)) }, args)
		when s_type_path c.cl_path = "Math" ->
		gen_math_call ctx cf args
	| TField (_, FStatic (c, cf))
		when s_type_path c.cl_path = "Math" && cf.cf_name = "PI" ->
		Imm "3.141592653589793"
	| TField (_, FEnum (_, ef)) ->
		(* Payload-free enum constructor = its tag index. Payload
			enums / match extraction are a v0.6 TODO (cf. sala). *)
		Imm (string_of_int ef.ef_index)
	| TLocal v -> begin
		try
			let slot = Hashtbl.find ctx.vars v.v_id in
			let r = fresh ctx "t" in
			let ty = mem_ty v.v_type in
			emit ctx (Printf.sprintf "%s = load %s+0 as %s" r slot ty);
			track ctx r;
			Reg r
		with Not_found ->
			comment ctx ("SA-TODO(v0.2): unhomed local " ^ v.v_name);
			Imm "0"
	end
	| TBinop (OpAssign, { eexpr = TLocal v }, rhs) -> begin
		try
			let slot = Hashtbl.find ctx.vars v.v_id in
			if is_string_t v.v_type then begin
				match materialize_owned ctx rhs with
				| Some (p, lop) ->
					emit ctx (Printf.sprintf "store %s+0, %s as ptr" slot p);
					Hashtbl.replace ctx.str_lens v.v_id lop;
					Imm p
				| None ->
					comment ctx "SA-TODO(v0.14): unmaterializable string assign";
					Imm "0"
			end else begin
			let op = gen_operand ctx rhs in
			let ty = mem_ty v.v_type in
			let os = match op with Imm s -> s | Reg r -> r in
			emit ctx (Printf.sprintf "store %s+0, %s as %s" slot os ty);
			op
			end
		with Not_found ->
			comment ctx ("SA-TODO(v0.2): assign to unhomed " ^ v.v_name);
			Imm "0"
	end
	| TBinop (OpAssignOp op, { eexpr = TLocal v }, rhs) -> begin
		(* Optimizer desugar: `x = x + e` arrives as OpAssignOp. *)
		try
			let slot = Hashtbl.find ctx.vars v.v_id in
			let ty = mem_ty v.v_type in
			let cur = fresh ctx "t" in
			emit ctx (Printf.sprintf "%s = load %s+0 as %s" cur slot ty);
			track ctx cur;
			let o = gen_operand ctx rhs in
			let os = match o with Imm s -> s | Reg r -> r in
			let mn = match op with
				| OpAdd -> "add" | OpSub -> "sub" | OpMult -> "mul"
				| OpDiv -> "sdiv" | OpMod -> "srem"
				| OpAnd -> "and" | OpOr -> "or" | OpXor -> "xor"
				| OpShl -> "shl" | OpShr -> "ashr" | OpUShr -> "lshr"
				| _ -> ""
			in
			if mn = "" then begin
				comment ctx ("SA-TODO(v0.3): assign-op " ^ Ast.s_binop op);
				Imm "0"
			end else begin
				let r = fresh ctx "t" in
				emit ctx (Printf.sprintf "%s = %s %s, %s" r mn cur os);
				track ctx r;
				emit ctx (Printf.sprintf "store %s+0, %s as %s" slot r ty);
				Reg r
			end
		with Not_found ->
			comment ctx ("SA-TODO(v0.2): assign-op to unhomed " ^ v.v_name);
			Imm "0"
	end
	| TBinop (OpAssignOp op, ({ eexpr = TField (obj, FInstance (c, _, cf)) } as lhs), rhs)
		when is_simple_base obj -> begin
		match field_offset c cf.cf_name, arith_mnemonic op with
		| Some off, Some mn ->
			let cur = gen_operand ctx lhs in
			let o = gen_operand ctx rhs in
			let cs = match cur with Imm s -> s | Reg r -> r in
			let os = match o with Imm s -> s | Reg r -> r in
			let r = fresh ctx "t" in
			emit ctx (Printf.sprintf "%s = %s %s, %s" r mn cs os);
			track ctx r;
			let bo = gen_operand ctx obj in
			let bs = match bo with Imm s -> s | Reg r -> r in
			emit ctx (Printf.sprintf "store %s+%d, %s as %s" bs off r (mem_ty cf.cf_type));
			Reg r
		| _ ->
			comment ctx "SA-TODO(v0.9): field assign-op";
			Imm "0"
	end
	| TBinop (OpAssignOp op, ({ eexpr = TField (obj, FAnon cf) } as lhs), rhs)
		when is_simple_base obj -> begin
		match arith_mnemonic op with
		| Some mn ->
			let cur = gen_operand ctx lhs in
			let o = gen_operand ctx rhs in
			let cs = match cur with Imm s -> s | Reg r -> r in
			let os = match o with Imm s -> s | Reg r -> r in
			let r = fresh ctx "t" in
			emit ctx (Printf.sprintf "%s = %s %s, %s" r mn cs os);
			track ctx r;
			let bo = gen_operand ctx obj in
			let bs = match bo with Imm s -> s | Reg r -> r in
			let off = match anon_offset obj.etype cf.cf_name with
				| Some o -> o | None -> 0 in
			emit ctx (Printf.sprintf "store %s+%d, %s as i32" bs off r);
			Reg r
		| None ->
			comment ctx "SA-TODO(v0.9): field assign-op";
			Imm "0"
	end
	| TBinop (OpAssignOp op, ({ eexpr = TArray (base, idx) } as lhs), rhs)
		when is_simple_base base -> begin
		match arith_mnemonic op with
		| Some mn ->
			let cur = gen_operand ctx lhs in
			let o = gen_operand ctx rhs in
			let cs = match cur with Imm s -> s | Reg r -> r in
			let os = match o with Imm s -> s | Reg r -> r in
			let r = fresh ctx "t" in
			emit ctx (Printf.sprintf "%s = %s %s, %s" r mn cs os);
			track ctx r;
			let bo = gen_operand ctx base in
			let io = gen_operand ctx idx in
			let bs = match bo with Imm s -> s | Reg r -> r in
			let iis = match io with Imm s -> s | Reg r -> r in
			let off = fresh ctx "off" in
			emit ctx (Printf.sprintf "%s = mul %s, 8" off iis);
			track ctx off;
			let p = fresh ctx "ep" in
			emit ctx (Printf.sprintf "%s = ptr_add %s, %s" p bs off);
			track ctx p;
			emit ctx (Printf.sprintf "store %s+0, %s as %s" p r (elem_ty base));
			Reg r
		| None ->
			comment ctx "SA-TODO(v0.9): array assign-op";
			Imm "0"
	end
	| TUnop (Neg, _, e1) when is_float_t e1.etype ->
		let o = gen_operand ctx e1 in
		let os = match o with Imm s -> s | Reg r -> r in
		let r = fresh ctx "t" in
		emit ctx (Printf.sprintf "%s = fneg %s" r os);
		track ctx r;
		Reg r
	| TUnop (op, _, { eexpr = TLocal v }) -> begin
		(* `i++` / `i--` (optimizer also rewrites `i = i + 1`). *)		try
			let slot = Hashtbl.find ctx.vars v.v_id in
			let ty = mem_ty v.v_type in
			let cur = fresh ctx "t" in
			emit ctx (Printf.sprintf "%s = load %s+0 as %s" cur slot ty);
			track ctx cur;
			let mn = match op with
				| Increment -> "add" | Decrement -> "sub" | _ -> ""
			in
			if mn = "" then begin
				comment ctx "SA-TODO(v0.4): prefix unop (neg/not)";
				Reg cur
			end else begin
				let r = fresh ctx "t" in
				emit ctx (Printf.sprintf "%s = %s %s, 1" r mn cur);
				track ctx r;
				emit ctx (Printf.sprintf "store %s+0, %s as %s" slot r ty);
				Reg r
			end
		with Not_found ->
			comment ctx ("SA-TODO: unop on unhomed " ^ v.v_name);
			Imm "0"
	end
	| TBinop (OpAssign, { eexpr = TArray (base, idx) }, rhs) ->
		gen_array_set ctx base idx rhs
	| TBinop (OpAssign, { eexpr = TField (obj, FAnon cf) }, rhs) ->
		gen_field_set ctx obj cf.cf_name rhs
	| TBinop (OpAssign, { eexpr = TField (obj, FInstance (c, _, cf)) }, rhs) ->
		gen_ifield_set ctx obj c cf rhs
	| TObjectDecl decls -> gen_object_decl ctx decls
	| TField (obj, FAnon cf) -> gen_field_get ctx obj cf.cf_name
	| TCall ({ eexpr = TField (_, FEnum (_, ef)) }, args) ->
		gen_enum_construct ctx ef args
	| TEnumParameter (e1, ef, index) -> gen_enum_param ctx e1 ef index
	| TCall ({ eexpr = TField (_, FStatic (c, cf)) }, [arg])
		when s_type_path c.cl_path = "Std" && cf.cf_name = "int" ->
		gen_std_int ctx arg
	| TCall ({ eexpr = TField (_, FStatic (c, cf)) }, _)
		when s_type_path c.cl_path = "Sys" && cf.cf_name = "time" ->
		sys_time_op ctx
	| TCall ({ eexpr = TField (_, FStatic (c, cf)) }, [port])
		when s_type_path c.cl_path = "sa.net.Tcp" && cf.cf_name = "listen" ->
		net_listen_op ctx (gen_operand ctx port)
	| TCall ({ eexpr = TField (_, FStatic (c, cf)) }, [lnr])
		when s_type_path c.cl_path = "sa.net.Tcp" && cf.cf_name = "boundPort" ->
		(match gen_operand ctx lnr with
		| Imm s -> net_bound_port_op ctx s
		| Reg r -> net_bound_port_op ctx r)
	| TCall ({ eexpr = TField (_, FStatic (c, cf)) }, [host; port])
		when s_type_path c.cl_path = "sa.net.Tcp" && cf.cf_name = "connect" ->
		net_tcp_connect_op ctx host port
	| TCall ({ eexpr = TField (_, FStatic (c, cf)) }, [stream; data])
		when s_type_path c.cl_path = "sa.net.Tcp" && cf.cf_name = "write" ->
		net_tcp_write_op ctx stream data
	| TCall ({ eexpr = TField (_, FStatic (c, cf)) }, [port])
		when s_type_path c.cl_path = "sa.net.Udp" && cf.cf_name = "bind" ->
		net_udp_bind_op ctx port
	| TCall ({ eexpr = TField (_, FStatic (c, cf)) }, [s])
		when s_type_path c.cl_path = "sa.net.Udp" && cf.cf_name = "port" ->
		(match gen_operand ctx s with
		| Imm x -> net_udp_port_op ctx x
		| Reg r -> net_udp_port_op ctx r)
	| TCall ({ eexpr = TField (_, FStatic (c, cf)) }, [s; data])
		when s_type_path c.cl_path = "sa.net.Udp" && cf.cf_name = "send" ->
		net_udp_send_op ctx s data
	| TCall ({ eexpr = TField (_, FStatic (c, cf)) }, args) ->
		(match sys_surface_operand ctx c cf args with
		| Some o -> o
		| None -> gen_call ctx c cf args)
	| TBinop (op, e1, e2) -> gen_binop ctx op e1 e2 e.etype
	| TParenthesis e1 | TMeta (_, e1) -> gen_operand ctx e1
	| TCast (e1, _) -> gen_operand ctx e1
	| TEnumIndex e1 ->
		(* Tag extraction: our payload-free enum values already ARE
			the tag (stored by FEnum arm), so lower the inner value.
			Payload enums (TEnumParameter) stay a v0.6 TODO. *)
		gen_operand ctx e1
	| TArrayDecl elems -> gen_array_decl ctx elems
	| TArray (base, idx) -> gen_array_get ctx base idx
	| TField (base, acc) when is_length_access base acc ->
		gen_array_len ctx base
	| TField (obj, FInstance (c, _, cf)) -> gen_ifield_get ctx obj c cf
	| _ ->
		comment ctx ("SA-TODO(v0.2/v0.3): unsupported expression " ^ expr_kind e);
		Imm "0"

and gen_ifield_get ctx obj c cf =
	match field_offset c cf.cf_name with
	| None ->
		comment ctx ("SA-TODO(v0.9): non-Var field " ^ cf.cf_name);
		Imm "0"
	| Some off ->
	match gen_operand ctx obj with
	| Imm _ ->
		comment ctx "SA-TODO(v0.9): object base must be a register";
		Imm "0"
	| Reg b ->
		let ty = mem_ty cf.cf_type in
		let r = fresh ctx "t" in
		emit ctx (Printf.sprintf "%s = load %s+%d as %s" r b off ty);
		track ctx r;
		Reg r

(** Lower `obj.field = v` writes. *)
and gen_ifield_set ctx obj c cf rhs =
	match field_offset c cf.cf_name with
	| None ->
		comment ctx ("SA-TODO(v0.9): non-Var field " ^ cf.cf_name);
		Imm "0"
	| Some off ->
	match gen_operand ctx obj with
	| Imm _ ->
		comment ctx "SA-TODO(v0.9): object base must be a register";
		Imm "0"
	| Reg b ->
		let o = gen_operand ctx rhs in
		let os = match o with Imm s -> s | Reg r -> r in
		emit ctx (Printf.sprintf "store %s+%d, %s as %s" b off os (mem_ty cf.cf_type));
		o

(** Lower `new C(args)` via the emitted constructor. *)
and gen_new ctx c args =
	let name = sa_ctor_name c in
	if not (Hashtbl.mem ctx.emitted name) then begin
		(match c.cl_constructor with
		| Some ccf when cf_is_bare_generic ccf -> generic_hint ctx c ccf
		| _ -> comment ctx ("SA-TODO(v0.9): non-emitted ctor " ^ s_type_path c.cl_path));
		Imm "0"
	end else begin
		let ss = match c.cl_constructor with
			| None -> Some (List.map (fun a ->
				match gen_operand ctx a with Imm s -> s | Reg r -> r) args)
			| Some ccf -> (match splice_args ctx ccf args with
				| Some s -> Some s
				| None ->
					comment ctx "SA-TODO(v0.13): unresolvable ctor argument";
					None) in
		match ss with
		| None -> Imm "0"
		| Some ss ->
		let r = fresh ctx "t" in
		emit ctx (Printf.sprintf "%s = call @%s(%s)" r name (String.concat ", " ss));
		track ctx r;
		Reg r
	end

(** Lower `obj.method(args)` via the emitted method (self first). *)
and gen_method_call ctx c cf obj args =
	let name = sa_fun_name c cf in
	if not (Hashtbl.mem ctx.emitted name) then begin
		if cf_is_bare_generic cf then generic_hint ctx c cf
		else comment ctx ("SA-TODO(v0.9): non-emitted method " ^
			s_type_path c.cl_path ^ "." ^ cf.cf_name);
		Imm "0"
	end else begin
		let so = gen_operand ctx obj in
		let ss = match so with Imm s -> s | Reg r -> r in
		match splice_args ctx cf args with
		| None ->
			comment ctx "SA-TODO(v0.13): unresolvable method argument";
			Imm "0"
		| Some rest ->
		let is_str_ret = match follow cf.cf_type with
			| TFun (_, r) -> is_string_t r
			| _ -> false in
		let rest = if is_str_ret then
			let ps = (match ctx.pslot with Some s -> s | None -> ensure_pslot ctx) in
			rest @ ["&" ^ ps]
		else rest in
		let ret = match follow cf.cf_type with
			| TFun (_, r) -> scalar_ret r
			| _ -> None in
		match ret with
		| Some "void" ->
			emit ctx (Printf.sprintf "call @%s(%s)" name (String.concat ", " (ss :: rest)));
			Imm "0"
		| _ ->
			let r = fresh ctx "t" in
			emit ctx (Printf.sprintf "%s = call @%s(%s)" r name (String.concat ", " (ss :: rest)));
			track ctx r;
			Reg r
	end

(** Array layout v0.4a (fixed-size, 8-byte slots):
	`+0` holds the length as u64, elements follow at `+8+i*8`.
	The read shape mirrors `ARRAY_GET_U64` in `sci/sa_std/array.sa`
	(`off = mul idx, 8; ptr = ptr_add base, off; load ptr+0`).
	`push`/growth needs the vec macros and is a v0.5 TODO. *)
(** Anonymous-object layout v0.4b: fields sorted by name, 8 bytes each.
	Declaration and access both use this order, so layouts always agree.
	Class instances need constructor calls and are a v0.5 TODO. *)
and anon_layout t =
	match follow t with
	| TAnon a ->
		List.sort String.compare
			(PMap.fold (fun cf acc -> cf.cf_name :: acc) a.a_fields [])
	| _ -> []

and anon_offset t name =
	let rec idx i = function
		| [] -> None
		| n :: ns -> if n = name then Some (i * 8) else idx (i + 1) ns
	in
	idx 0 (anon_layout t)

and gen_object_decl ctx decls =
	let names = List.sort String.compare
		(List.map (fun ((n, _, _), _) -> n) decls) in
	let n = List.length names in
	let obj = fresh ctx "obj" in
	emit ctx (Printf.sprintf "%s = alloc %d" obj (max n 1 * 8));
	track ctx obj;
	List.iteri (fun i name ->
		let ee = List.find (fun ((n, _, _), _) -> n = name) decls |> snd in
		let o = gen_operand ctx ee in
		let os = match o with Imm s -> s | Reg r -> r in
		emit ctx (Printf.sprintf "store %s+%d, %s as %s" obj (i * 8) os (mem_ty ee.etype))
	) names;
	Reg obj

and gen_field_get ctx obj cf_name =
	match anon_offset obj.etype cf_name with
	| None ->
		comment ctx ("SA-TODO(v0.5): class-instance field " ^ cf_name);
		Imm "0"
	| Some off ->
	(match gen_operand ctx obj with
	| Imm _ ->
		comment ctx "SA-TODO(v0.4): object base must be a register";
		Imm "0"
	| Reg b ->
		let r = fresh ctx "t" in
		emit ctx (Printf.sprintf "%s = load %s+%d as i32" r b off);
		track ctx r;
		Reg r)

and gen_field_set ctx obj cf_name rhs =
	match obj.eexpr with
	| TArray _ ->
		comment ctx "SA-TODO(v0.5): field write into array element (needs address)";
		Imm "0"
	| _ ->
	match anon_offset obj.etype cf_name with
	| None ->
		comment ctx ("SA-TODO(v0.5): class-instance field " ^ cf_name);
		Imm "0"
	| Some off ->
	(match gen_operand ctx obj with
	| Imm _ ->
		comment ctx "SA-TODO(v0.4): object base must be a register";
		Imm "0"
	| Reg b ->
		let o = gen_operand ctx rhs in
		let os = match o with Imm s -> s | Reg r -> r in
		emit ctx (Printf.sprintf "store %s+%d, %s as i32" b off os);
		o)
and elem_ty base =
	match follow base.etype with
	| TInst ({ cl_path = ([], "Array") }, [t]) -> mem_ty t
	| _ -> "i32"

and gen_array_decl ctx elems =
	let n = List.length elems in
	let arr = fresh ctx "arr" in
	emit ctx (Printf.sprintf "%s = alloc %d" arr ((n + 1) * 8));
	track ctx arr;
	emit ctx (Printf.sprintf "store %s+0, %d as u64" arr n);
	List.iteri (fun i ee ->
		let o = gen_operand ctx ee in
		let os = match o with Imm s -> s | Reg r -> r in
		let ty = mem_ty ee.etype in
		emit ctx (Printf.sprintf "store %s+%d, %s as %s" arr ((i + 1) * 8) os ty)
	) elems;
	Reg arr

and gen_array_ptr ctx base idx =
	let ob = gen_operand ctx base in
	let oi = gen_operand ctx idx in
	match ob with
	| Imm _ ->
		comment ctx "SA-TODO(v0.4): array base must be a register";
		None
	| Reg b ->
		let si = match oi with Imm s -> s | Reg r -> r in
		let off = fresh ctx "off" in
		emit ctx (Printf.sprintf "%s = mul %s, 8" off si);
		track ctx off;
		let p = fresh ctx "ep" in
		emit ctx (Printf.sprintf "%s = ptr_add %s, %s" p b off);
		track ctx p;
		Some p

and gen_array_get ctx base idx =
	match gen_array_ptr ctx base idx with
	| None -> Imm "0"
	| Some p ->
		let r = fresh ctx "t" in
		emit ctx (Printf.sprintf "%s = load %s+0 as %s" r p (elem_ty base));
		track ctx r;
		Reg r

and gen_array_set ctx base idx rhs =
	match gen_array_ptr ctx base idx with
	| None -> Imm "0"
	| Some p ->
		let o = gen_operand ctx rhs in
		let os = match o with Imm s -> s | Reg r -> r in
		emit ctx (Printf.sprintf "store %s+0, %s as %s" p os (elem_ty base));
		o

and is_length_access base acc =
	(match follow base.etype with
	| TInst ({ cl_path = ([], "Array") }, _) -> true
	| _ -> false)
	&& (match acc with
	| FDynamic "length" -> true
	| FInstance (_, _, f) when f.cf_name = "length" -> true
	| FAnon f when f.cf_name = "length" -> true
	| FStatic (_, f) when f.cf_name = "length" -> true
	| _ -> false)

and gen_array_len ctx base =
	match gen_operand ctx base with
	| Imm _ ->
		comment ctx "SA-TODO(v0.4): array base must be a register";
		Imm "0"
	| Reg b ->
		let r = fresh ctx "t" in
		emit ctx (Printf.sprintf "%s = load %s+0 as u64" r b);
		track ctx r;
		Reg r

(** Bare payload-free enum constructor (tag constant, no heap). *)
and is_tag_const e =
	match e.eexpr with
	| TField (_, FEnum _) -> true
	| TParenthesis e1 | TMeta (_, e1) | TCast (e1, _) -> is_tag_const e1
	| _ -> false

and gen_binop ctx op e1 e2 etype =
	if (op = OpEq || op = OpNotEq)
		&& (is_enum_t e1.etype || is_enum_t e2.etype)
		&& not (is_null_expr e1 || is_null_expr e2)
		&& not (is_tag_const e1 && is_tag_const e2) then
		gen_enum_eq ctx op e1 e2
	else if (op = OpEq || op = OpNotEq)
		&& (is_string_t e1.etype || is_string_t e2.etype)
		&& not (is_null_expr e1 || is_null_expr e2) then
		gen_string_eq ctx op e1 e2
	else begin
	let float_ctx = is_float_t e1.etype || is_float_t e2.etype || is_float_t etype in
	let int_mnemonic = match op with
		| OpAdd -> Some "add" | OpSub -> Some "sub" | OpMult -> Some "mul"
		| OpDiv -> Some "sdiv" | OpMod -> Some "srem"
		| OpEq -> Some "eq" | OpNotEq -> Some "ne"
		| OpGt -> Some "sgt" | OpGte -> Some "sge"
		| OpLt -> Some "slt" | OpLte -> Some "sle"
		| OpAnd -> Some "and" | OpOr -> Some "or" | OpXor -> Some "xor"
		| OpShl -> Some "shl" | OpShr -> Some "ashr" | OpUShr -> Some "lshr"
		| OpBoolAnd -> Some "and" | OpBoolOr -> Some "or"
		| _ -> None
	in
	let float_mnemonic = match op with
		| OpAdd -> Some "fadd" | OpSub -> Some "fsub"
		| OpMult -> Some "fmul" | OpDiv -> Some "fdiv"
		| OpEq -> Some "fcmp_eq" | OpNotEq -> Some "fcmp_ne"
		| OpGt -> Some "fcmp_gt" | OpGte -> Some "fcmp_ge"
		| OpLt -> Some "fcmp_lt" | OpLte -> Some "fcmp_le"
		| _ -> None
	in
	let mnemonic =
		if float_ctx then float_mnemonic
		else int_mnemonic
	in
	match mnemonic with
	| None ->
		comment ctx ("SA-TODO(v0.2): unsupported operator " ^ Ast.s_binop op ^
			(if float_ctx then " (float)" else " (int)"));
		Imm "0"
	| Some mn ->
		let o1 = gen_operand ctx e1 in
		let o2 = gen_operand ctx e2 in
		let s1 = match o1 with Imm s -> s | Reg r -> r in
		let s2 = match o2 with Imm s -> s | Reg r -> r in
		(* Mixed int/float: Haxe unifies to Float; convert int sides
			explicitly (SA has no implicit conversion). Int literals
			are fine as-is only when both sides are ints. *)
		let s1 = if float_ctx && is_int_t e1.etype then begin
			let r = fresh ctx "t" in
			emit ctx (Printf.sprintf "%s = sitofp %s" r s1);
			track ctx r;
			r
		end else s1 in
		let s2 = if float_ctx && is_int_t e2.etype then begin
			let r = fresh ctx "t" in
			emit ctx (Printf.sprintf "%s = sitofp %s" r s2);
			track ctx r;
			r
		end else s2 in
		let r = fresh ctx "t" in
		emit ctx (Printf.sprintf "%s = %s %s, %s" r mn s1 s2);
		track ctx r;
		Reg r
	end

(** String pair resolution for chain-internal callers: static pairs
	first, else String-returning calls (emitted with `&pslot`, length
	read back immediately). *)
and string_pair_or_call ctx e =
	match string_operands ctx e with
	| Some (p, l) ->
		let lop = (try ignore (int_of_string l); Imm l with _ -> Reg l) in
		Some (p, lop)
	| None -> match e.eexpr with
	| TCall ({ eexpr = TField (_, FStatic (c, cf)) }, args) -> begin
		match follow cf.cf_type with
		| TFun (_, r) when is_string_t r ->
			if not (Hashtbl.mem ctx.emitted (sa_fun_name c cf)) then None
			else begin match gen_call ctx c cf args with
			| Reg p ->
				let ps = (match ctx.pslot with Some s -> s | None -> ensure_pslot ctx) in
				let l = fresh ctx "t" in
				emit ctx (Printf.sprintf "%s = load %s+0 as u64" l ps);
				track ctx l;
				Some (p, Reg l)
			| Imm _ -> None
			end
		| _ -> None
	end
	| TCall ({ eexpr = TField (obj, FInstance (c, _, cf)) }, args) -> begin
		match follow cf.cf_type with
		| TFun (_, r) when is_string_t r ->
			if not (Hashtbl.mem ctx.emitted (sa_fun_name c cf)) then None
			else begin match gen_method_call ctx c cf obj args with
			| Reg p ->
				let ps = (match ctx.pslot with Some s -> s | None -> ensure_pslot ctx) in
				let l = fresh ctx "t" in
				emit ctx (Printf.sprintf "%s = load %s+0 as u64" l ps);
				track ctx l;
				Some (p, Reg l)
			| Imm _ -> None
			end
		| _ -> None
	end
	| _ -> None

(** v0.24b `Math.*` over supplemented `sa_math_*` contracts.
	`round` lowers to `floor(x + 0.5)` (Haxe semantics, no new ABI). *)
and gen_math_call ctx cf args =
	match cf.cf_name, args with
	| ("floor" | "ceil"), [x] ->
		let vs = match gen_operand ctx x with
			| Imm s -> s | Reg r -> r in
		let f = fresh ctx "t" in
		emit ctx (Printf.sprintf "%s = call @sa_math_%s(%s)"
			f (if cf.cf_name = "floor" then "floor" else "ceil") vs);
		track ctx f;
		let r = fresh ctx "t" in
		emit ctx (Printf.sprintf "%s = fptosi %s" r f);
		track ctx r;
		release_now ctx f;
		Reg r
	| "round", [x] ->
		let vs = match gen_operand ctx x with
			| Imm s -> s | Reg r -> r in
		let a = fresh ctx "t" in
		emit ctx (Printf.sprintf "%s = fadd %s, 0.5" a vs);
		track ctx a;
		let f = fresh ctx "t" in
		emit ctx (Printf.sprintf "%s = call @sa_math_floor(%s)" f a);
		track ctx f;
		let r = fresh ctx "t" in
		emit ctx (Printf.sprintf "%s = fptosi %s" r f);
		track ctx r;
		release_now ctx a;
		release_now ctx f;
		Reg r
	| ("sqrt" | "sin" | "cos"), [x] ->
		let vs = match gen_operand ctx x with
			| Imm s -> s | Reg r -> r in
		let r = fresh ctx "t" in
		emit ctx (Printf.sprintf "%s = call @sa_math_%s(%s)" r cf.cf_name vs);
		track ctx r;
		Reg r
	| "pow", [b; e] ->
		let bs = match gen_operand ctx b with
			| Imm s -> s | Reg r -> r in
		let es = match gen_operand ctx e with
			| Imm s -> s | Reg r -> r in
		let r = fresh ctx "t" in
		emit ctx (Printf.sprintf "%s = call @sa_math_pow(%s, %s)" r bs es);
		track ctx r;
		Reg r
	| "random", [] ->
		let r = fresh ctx "t" in
		emit ctx (Printf.sprintf "%s = call @sa_math_random()" r);
		track ctx r;
		Reg r
	| _ ->
		comment ctx ("SA-TODO(v0.24): Math." ^ cf.cf_name);
		Imm "0"

and gen_string_eq ctx op e1 e2 =
	match string_pair_or_call ctx e1, string_pair_or_call ctx e2 with
	| Some (p1, l1), Some (p2, l2) ->
		let r = fresh ctx "t" in
		let mn = if op = OpEq then "STRING_EQ" else "STRING_NEQ" in
		emit ctx (Printf.sprintf "EXPAND %s %s, %s, %s, %s, %s" mn r p1 (ops l1) p2 (ops l2));
		track ctx r;
		Reg r
	| _ ->
		comment ctx "SA-TODO(v0.6): string compare needs tracked lengths";
		Imm "0"

(** Materialize any string expression to a (ptr, len-operand) pair.
	Literals/views resolve without copying; concat/fmt build an owned
	heap (tracked for exit release) with handles freed eagerly. All
	contracts pre-exist in sci/sa_std. Stored results make
	`var s = a + b` work; immediate callers use the pair directly. *)
and materialize_owned ctx e : (string * operand) option =
	match string_operands ctx e with
	| Some (p, l) ->
		let lop = (try ignore (int_of_string l); Imm l with _ -> Reg l) in
		Some (p, lop)
	| None -> match e.eexpr with
	| TCall ({ eexpr = TField (_, FStatic (c, cf)) }, [arg])
		when s_type_path c.cl_path = "Std" && cf.cf_name = "string" ->
		materialize_fmt ctx arg
	| TCall ({ eexpr = TField (_, FStatic (c, cf)) }, n :: _)
		when s_type_path c.cl_path = "Sys" && cf.cf_name = "getEnv" ->
		materialize_env_get ctx n
	| TCall ({ eexpr = TField (_, FStatic (c, cf)) }, [s; maxv])
		when s_type_path c.cl_path = "sa.net.Udp" && cf.cf_name = "recv" ->
		(match gen_operand ctx s with
		| Imm _ -> None
		| Reg r ->
			let ms = match gen_operand ctx maxv with
				| Imm x -> x | Reg rr -> rr in
			let buf = fresh ctx "t" in
			emit ctx (Printf.sprintf "%s = alloc %s" buf ms);
			track ctx buf;
			let st = fresh ctx "t" in
			let n = fresh ctx "t" in
			emit ctx (Printf.sprintf "EXPAND NET_UDP_RECV %s, %s, %s, %s, %s"
				st n r buf ms);
			track ctx st;
			track ctx n;
			panic_unless_ok ctx st "haxe:net-udp-recv";
			release_now ctx st;
			Some (buf, Reg n))
	| TCall ({ eexpr = TField (_, FStatic (c, cf)) }, _)
		when s_type_path c.cl_path = "Sys" && cf.cf_name = "getCwd" ->
		materialize_cwd ctx
	| TBinop (OpAdd, l, r) when is_string_t e.etype ->
		materialize_concat_owned ctx l r
	| TParenthesis e1 | TMeta (_, e1) | TCast (e1, _) ->
		materialize_owned ctx e1
	| TCall _ -> string_pair_or_call ctx e
	| _ -> None

(** Owned copy of `Sys.getEnv` / `Sys.getCwd` (miss panics, like trace). *)
and materialize_env_get ctx n =
	match string_operands ctx n with
	| Some (kp, kl) ->
		let h = fresh ctx "h" in
		emit ctx (Printf.sprintf "%s = call @sa_env_get(%s, %s)" h kp kl);
		track ctx h;
		let is0 = fresh ctx "t" in
		emit ctx (Printf.sprintf "%s = eq %s, 0" is0 h);
		track ctx is0;
		let l_miss = fresh_label ctx "EMISS" in
		let l_hit = fresh_label ctx "EHIT" in
		emit ctx (Printf.sprintf "br %s -> %s, %s" is0 l_miss l_hit);
		emit_label ctx l_miss;
		emit ctx "panic(\"haxe:env-missing\")";
		emit_label ctx l_hit;
		release_now ctx is0;
		Some (owned_of_handle ctx h
			"sa_env_buffer_data" "sa_env_buffer_len" "sa_env_buffer_free")
	| None -> None

and materialize_cwd ctx =
	let h = fresh ctx "h" in
	emit ctx (Printf.sprintf "%s = call @sa_env_current_dir()" h);
	track ctx h;
	Some (owned_of_handle ctx h
		"sa_env_buffer_data" "sa_env_buffer_len" "sa_env_buffer_free")

(** Owned copy of a registry handle (fmt/concat/env/fs): data/len out,
	bytes copied to fresh heap, handle freed. Returns (own, Reg len). *)and owned_of_handle ctx h data_fn len_fn free_fn =
	let d = fresh ctx "t" in
	emit ctx (Printf.sprintf "%s = call @%s(%s)" d data_fn h);
	track ctx d;
	let l = fresh ctx "t" in
	emit ctx (Printf.sprintf "%s = call @%s(%s)" l len_fn h);
	track ctx l;
	let own = fresh ctx "t" in
	emit ctx (Printf.sprintf "%s = alloc %s" own l);
	track ctx own;
	emit ctx (Printf.sprintf "call @sa_mem_copy(&%s, &%s, %s)" own d l);
	let f = fresh ctx "t" in
	emit ctx (Printf.sprintf "%s = call @%s(^%s)" f free_fn h);
	track ctx f;
	forget ctx h;
	release_now ctx d;
	release_now ctx f;
	(own, Reg l)

and materialize_fmt ctx arg =
	match follow arg.etype with
	| TAbstract ({ a_path = ([], "Int") }, _) ->
		let vs = match gen_operand ctx arg with
			| Imm s -> s | Reg r -> r in
		let h = fresh ctx "h" in
		emit ctx (Printf.sprintf "%s = call @sa_fmt_i64(%s, 10)" h vs);
		track ctx h;
		Some (owned_of_handle ctx h "sa_fmt_buffer_data" "sa_fmt_buffer_len" "sa_fmt_buffer_free")
	| TAbstract ({ a_path = ([], "Float") }, _) ->
		let vs = match gen_operand ctx arg with
			| Imm s -> s | Reg r -> r in
		let h = fresh ctx "h" in
		emit ctx (Printf.sprintf "%s = call @sa_fmt_f64(%s, 6)" h vs);
		track ctx h;
		Some (owned_of_handle ctx h "sa_fmt_buffer_data" "sa_fmt_buffer_len" "sa_fmt_buffer_free")
	| _ -> None

and materialize_concat_owned ctx l r =
	let rec flatten acc e = match e.eexpr with
		| TBinop (OpAdd, a, b) when is_string_t e.etype ->
			flatten (flatten acc a) b
		| TParenthesis e1 | TMeta (_, e1) | TCast (e1, _) -> flatten acc e1
		| _ -> acc @ [e] in
	let parts = flatten (flatten [] l) r in
	let resolved = List.map (materialize_owned ctx) parts in
	if List.length parts < 2 || List.exists ((=) None) resolved then None
	else begin
		let pairs = List.map (function Some x -> x | None -> ("", Imm "0")) resolved in
		let opstr = function Imm s -> s | Reg rr -> rr in
		let (p0, l0) = List.hd pairs in
		let cur_h = ref "" in
		let step pa la pb lb first =
			let h = fresh ctx "h" in
			emit ctx (Printf.sprintf "%s = call @sa_string_concat(%s, %s, %s, %s)"
				h pa la pb lb);
			track ctx h;
			if not first then begin
				let f = fresh ctx "t" in
				emit ctx (Printf.sprintf "%s = call @sa_fmt_buffer_free(^%s)" f !cur_h);
				track ctx f;
				release_now ctx f;
				forget ctx !cur_h
			end;
			cur_h := h in
		(match pairs with
		| (p0, l0) :: (p1, l1) :: rest ->
			step p0 (opstr l0) p1 (opstr l1) true;
			List.iter (fun (pn, ln) ->
				let d = fresh ctx "t" in
				emit ctx (Printf.sprintf "%s = call @sa_fmt_buffer_data(%s)" d !cur_h);
				track ctx d;
				let ln2 = fresh ctx "t" in
				emit ctx (Printf.sprintf "%s = call @sa_fmt_buffer_len(%s)" ln2 !cur_h);
				track ctx ln2;
				let h2 = fresh ctx "h" in
				emit ctx (Printf.sprintf "%s = call @sa_string_concat(%s, %s, %s, %s)"
					h2 d ln2 pn (opstr ln));
				track ctx h2;
				let f = fresh ctx "t" in
				emit ctx (Printf.sprintf "%s = call @sa_fmt_buffer_free(^%s)" f !cur_h);
				track ctx f;
				release_now ctx d;
				release_now ctx ln2;
				release_now ctx f;
				forget ctx !cur_h;
				cur_h := h2
			) rest
		| _ -> ());
		if !cur_h = "" then None
		else Some (owned_of_handle ctx !cur_h
			"sa_fmt_buffer_data" "sa_fmt_buffer_len" "sa_fmt_buffer_free")
	end

(** Print a computed string directly without storing it.
	`Std.string(int)` flows through `sa_fmt_i64`, `a + b` through
	`sa_string_concat`; both return registry handles read via the
	shared `sa_fmt_buffer_*` triple and freed inline, so no new ABI.
	Concat sides must resolve via string_operands (literals/tracked
	vars); stored computed strings stay a TODO. Returns true when
	emitted. *)
and trace_string_value ctx e : bool =
	match e.eexpr with
	| TCall ({ eexpr = TField (_, FStatic (c, cf)) }, [arg])
		when s_type_path c.cl_path = "Std" && cf.cf_name = "string" ->
		trace_fmt_int ctx arg
	| TBinop (OpAdd, l, r) when is_string_t e.etype ->
		trace_concat ctx l r
	| TParenthesis e1 | TMeta (_, e1) | TCast (e1, _) -> trace_string_value ctx e1
	| _ -> match string_pair_or_call ctx e with
		| Some (p, l) ->
			let ps = (match ctx.pslot with Some s -> s | None -> ensure_pslot ctx) in
			emit ctx (Printf.sprintf "store %s+0, %s as ptr" ps p);
			emit ctx (Printf.sprintf "call @sa_print_bytes(&%s, %s)" ps (ops l));
			true
		| None -> false

and trace_buffered ctx h =
	let ps = match ctx.pslot with Some s -> s | None -> ensure_pslot ctx in
	let d = fresh ctx "t" in
	emit ctx (Printf.sprintf "%s = call @sa_fmt_buffer_data(%s)" d h);
	track ctx d;
	let l = fresh ctx "t" in
	emit ctx (Printf.sprintf "%s = call @sa_fmt_buffer_len(%s)" l h);
	track ctx l;
	emit ctx (Printf.sprintf "store %s+0, %s as ptr" ps d);
	emit ctx (Printf.sprintf "call @sa_print_bytes(&%s, %s)" ps l);
	let f = fresh ctx "t" in
	emit ctx (Printf.sprintf "%s = call @sa_fmt_buffer_free(^%s)" f h);
	track ctx f;
	forget ctx h;
	release_now ctx d;
	release_now ctx l;
	release_now ctx f;
	true

and trace_fmt_int ctx arg =
	match follow arg.etype with
	| TAbstract ({ a_path = ([], "Int") }, _) ->
		let vs = match gen_operand ctx arg with
			| Imm s -> s | Reg r -> r in
		let h = fresh ctx "h" in
		emit ctx (Printf.sprintf "%s = call @sa_fmt_i64(%s, 10)" h vs);
		track ctx h;
		trace_buffered ctx h
	| TAbstract ({ a_path = ([], "Float") }, _) ->
		(* ts-plugin precedent: precision 6. Repr differs from Haxe's
			shortest-round-tripNan (e.g. 1.5 -> "1.500000"); documented. *)
		let vs = match gen_operand ctx arg with
			| Imm s -> s | Reg r -> r in
		let h = fresh ctx "h" in
		emit ctx (Printf.sprintf "%s = call @sa_fmt_f64(%s, 6)" h vs);
		track ctx h;
		trace_buffered ctx h
	| _ ->
		comment ctx "SA-TODO(v0.6): Std.string(non-numeric)";
		false

(** `Std.int(x)`: float truncation toward zero (fptosi); ints pass
	through; anything else is an honest TODO. *)
and gen_std_int ctx arg =
	match follow arg.etype with
	| TAbstract ({ a_path = ([], "Float") }, _) ->
		let vs = match gen_operand ctx arg with
			| Imm s -> s | Reg r -> r in
		let r = fresh ctx "t" in
		emit ctx (Printf.sprintf "%s = fptosi %s" r vs);
		track ctx r;
		Reg r
	| TAbstract ({ a_path = ([], "Int") }, _) ->
		gen_operand ctx arg
	| _ ->
		comment ctx "SA-TODO(v0.12): Std.int(non-numeric)";
		Imm "0"

and trace_concat ctx l r =
	let rec flatten acc e = match e.eexpr with
		| TBinop (OpAdd, a, b) when is_string_t e.etype ->
			flatten (flatten acc a) b
		| TParenthesis e1 | TMeta (_, e1) | TCast (e1, _) -> flatten acc e1
		| _ -> acc @ [e] in
	let parts = flatten (flatten [] l) r in
	let resolved = List.map (string_pair_or_call ctx) parts in
	if List.length parts < 2 || List.exists ((=) None) resolved then begin
		comment ctx "SA-TODO(v0.6): concat needs resolvable sides";
		false
	end else begin
		let pairs = List.map (function Some (p, l) -> (p, ops l) | None -> ("", "")) resolved in
		let cur = ref "" in
		let do_concat h pa la pb lb =
			emit ctx (Printf.sprintf "%s = call @sa_string_concat(%s, %s, %s, %s)"
				h pa la pb lb) in
		(match pairs with
		| (p0, l0) :: (p1, l1) :: rest ->
			let h0 = fresh ctx "h" in
			track ctx h0;
			do_concat h0 p0 l0 p1 l1;
			cur := h0;
			List.iter (fun (pn, ln) ->
				let d = fresh ctx "t" in
				emit ctx (Printf.sprintf "%s = call @sa_fmt_buffer_data(%s)" d !cur);
				track ctx d;
				let ln2 = fresh ctx "t" in
				emit ctx (Printf.sprintf "%s = call @sa_fmt_buffer_len(%s)" ln2 !cur);
				track ctx ln2;
				let h2 = fresh ctx "h" in
				emit ctx (Printf.sprintf "%s = call @sa_string_concat(%s, %s, %s, %s)"
					h2 d ln2 pn ln);
				track ctx h2;
				let f = fresh ctx "t" in
				emit ctx (Printf.sprintf "%s = call @sa_fmt_buffer_free(^%s)" f !cur);
				track ctx f;
				release_now ctx d;
				release_now ctx ln2;
				release_now ctx f;
				forget ctx !cur;
				cur := h2
			) rest
		| _ -> ());
		if !cur = "" then false else trace_buffered ctx !cur
	end

and sys_sleep ctx s =
	let vs = match gen_operand ctx s with
		| Imm x -> x | Reg r -> r in
	let ms = fresh ctx "t" in
	emit ctx (Printf.sprintf "%s = fmul %s, 1000.0" ms vs);
	track ctx ms;
	let mi = fresh ctx "t" in
	emit ctx (Printf.sprintf "%s = fptosi %s" mi ms);
	track ctx mi;
	let st = fresh ctx "t" in
	emit ctx (Printf.sprintf "%s = call @sa_time_sleep_ms(%s)" st mi);
	track ctx st;
	release_now ctx ms;
	release_now ctx mi;
	release_now ctx st;
	true

and gen_trace ctx args =
	if gen_trace_lit ctx args then ()
	else match args with
	| a :: _ ->
		if trace_string_value ctx a then () else begin
			match a.eexpr with
			| TCall ({ eexpr = TField (_, FStatic (c, cf)) }, p :: _)
				when s_type_path c.cl_path = "sys.io.File" && cf.cf_name = "getContent" ->
				if trace_fs_get_content ctx p then () else
					comment ctx "SA-TODO(v0.11): getContent needs resolvable path"
			| TCall ({ eexpr = TField (_, FStatic (c, cf)) }, n :: _)
				when s_type_path c.cl_path = "Sys" && cf.cf_name = "getEnv" ->
				if trace_sys_getenv ctx n then () else
					comment ctx "SA-TODO(v0.11): getEnv needs resolvable name"
			| TCall ({ eexpr = TField (_, FStatic (c, cf)) }, _)
				when s_type_path c.cl_path = "Sys" && cf.cf_name = "getCwd" ->
				ignore (trace_sys_getcwd ctx)
			| _ ->
				comment ctx "SA-TODO(v0.6): trace needs printable value"
		end
	| [] -> ()

and entry_stmts e =
	match e.eexpr with
	| TBlock el ->
		List.fold_left (fun (ss, a, c) s ->
			let (ss2, a2, c2) = entry_stmts s in
			(ss @ ss2, a || a2, match c with Some _ -> c | None -> c2)
		) ([], false, None) el
	| TMeta (_, e1) | TParenthesis e1 | TCast (e1, _) -> entry_stmts e1
	| TCall ({ eexpr = TField (_, FStatic (c, cf)) }, _) -> begin
		match cf.cf_expr with
		| Some { eexpr = TFunction tf } ->
			let body = match tf.tf_expr.eexpr with
				| TBlock el -> el
				| _ -> [tf.tf_expr]
			in
			(body, tf.tf_args <> [], Some c)
		| Some other -> ([other], false, Some c)
		| _ -> ([], false, None)
	end
	| TFunction tf -> ([tf.tf_expr], false, None)
	| _ -> ([e], false, None)

(** Callee expression names a `trace`-like function. Covers both the
	global `trace(...)` call and `haxe.Log.trace(...)`. *)
and callee_is_trace e =
	match e.eexpr with
	| TIdent "trace" -> true
	| TField (_, FStatic (_, { cf_name = "trace" })) -> true
	| TField (_, FInstance (_, _, { cf_name = "trace" })) -> true
	| TParenthesis e1 | TMeta (_, e1) | TCast (e1, _) -> callee_is_trace e1
	| _ -> false

and collect_vars acc e =
	match e.eexpr with
	| TVar (v, _) -> v :: acc
	| TBlock el -> List.fold_left collect_vars acc el
	| TIf (c, t, eo) ->
		let acc = collect_vars acc c in
		let acc = collect_vars acc t in
		(match eo with Some x -> collect_vars acc x | None -> acc)
	| TWhile (c, b, _) -> collect_vars (collect_vars acc c) b
	| TTry (e1, catches) ->
		List.fold_left (fun a (_, e2) -> collect_vars a e2)
			(collect_vars acc e1) catches
	| TSwitch sw ->
		let acc = collect_vars acc sw.switch_subject in
		let acc = List.fold_left (fun a c ->
			let a = List.fold_left collect_vars a c.case_patterns in
			collect_vars a c.case_expr) acc sw.switch_cases in
		(match sw.switch_default with Some x -> collect_vars acc x | None -> acc)
	| TParenthesis e1 | TMeta (_, e1) | TCast (e1, _) -> collect_vars acc e1
	| _ -> acc

(** True when the loop body can directly `break`/`continue` THIS loop
	(jumps inside a nested `while` or a closure belong to it).
	Used to guard `do-while`: its first iteration runs before any
	condition temps exist, so a direct jump cannot be merged
	Phi-consistently and falls back to an honest TODO. *)
and has_direct_jump depth e =
	match e.eexpr with
	| TBreak | TContinue -> depth = 0
	| TWhile _ -> false
	| TFunction _ -> false
	| TTry (e1, catches) ->
		has_direct_jump depth e1
		|| List.exists (fun (_, e2) -> has_direct_jump depth e2) catches
	| TBlock el -> List.exists (has_direct_jump depth) el
	| TIf (c, t, eo) ->
		has_direct_jump depth c || has_direct_jump depth t
		|| (match eo with Some x -> has_direct_jump depth x | None -> false)
	| TSwitch sw ->
		has_direct_jump depth sw.switch_subject
		|| List.exists (fun c ->
			List.exists (has_direct_jump depth) c.case_patterns
			|| has_direct_jump depth c.case_expr) sw.switch_cases
		|| (match sw.switch_default with
			| Some x -> has_direct_jump depth x | None -> false)
	| TParenthesis e1 | TMeta (_, e1) | TCast (e1, _) -> has_direct_jump depth e1
	| _ -> false

(** Integer-like switch pattern (int/bool/enum-tag constants).
	String patterns need content equality (v0.6); anything else is
	exotic (guards, payloads) and rejects the whole switch honestly. *)
and switch_pat_const e =
	match e.eexpr with
	| TConst (TInt i) -> Some (Int32.to_string i)
	| TConst (TBool b) -> Some (if b then "1" else "0")
	| TField (_, FEnum (_, ef)) -> Some (string_of_int ef.ef_index)
	| TParenthesis e1 | TMeta (_, e1) | TCast (e1, _) -> switch_pat_const e1
	| _ -> None

(** `switch` -> `eq` + `br` chains (see sala 06_limitations: no
	structured switch in SA). Subject evaluated once; ALL arms release
	to the pre-subject snapshot so every edge into the merge label
	carries the same live set. String subjects use the supplemented
	`STRING_EQ` macro via per-pattern EXPANDs. *)
(** Condition register: `br` needs a register, never an immediate
	(failed lowerings yield Imm fallbacks that must be bound). *)
and cond_reg ctx e =
	match gen_operand ctx e with
	| Reg r -> r
	| Imm s ->
		let r = fresh ctx "t" in
		emit ctx (Printf.sprintf "%s = add %s, 0" r s);
		track ctx r;
		r

(** Payload type of an enum constructor field by index (None when
	unresolvable — callers fall back honestly instead of guessing). *)
and enum_payload_ty ef index =
	match follow ef.ef_type with
	| TFun (args, _) -> begin
		try
			let (_, _, t) = List.nth args index in
			Some (mem_ty t)
		with _ -> None
	end
	| _ -> None

(** Construct a payload enum value: tagged heap object
	`[tag:i32][payload0][payload1]...` (8 bytes each). Payload-free
	uses the existing tag-constant path. *)
and gen_enum_construct ctx ef args =
	let tys = List.mapi (fun i _ -> enum_payload_ty ef i) args in
	if List.exists ((=) None) tys then begin
		comment ctx "SA-TODO(v0.19): unresolvable payload types";
		Imm "0"
	end else begin
		let n = List.length args in
		let obj = fresh ctx "obj" in
		emit ctx (Printf.sprintf "%s = alloc %d" obj ((n + 1) * 8));
		track ctx obj;
		emit ctx (Printf.sprintf "store %s+0, %d as i32" obj ef.ef_index);
		let rec store_each i = function
			| [], [] -> ()
			| a :: rest_a, tyo :: rest_t ->
				let o = gen_operand ctx a in
				let os = match o with Imm s -> s | Reg r -> r in
				let ty = match tyo with Some t -> t | None -> "i32" in
				emit ctx (Printf.sprintf "store %s+%d, %s as %s"
					obj ((i + 1) * 8) os ty);
				store_each (i + 1) (rest_a, rest_t)
			| _ -> () in
		store_each 0 (args, tys);
		Reg obj
	end

(** Extract a payload word: `load obj+(8+i*8)`. *)
and gen_enum_param ctx e ef index =
	match gen_operand ctx e with
	| Imm _ ->
		comment ctx "SA-TODO(v0.19): enum base must be a register";
		Imm "0"
	| Reg b ->
	(match enum_payload_ty ef index with
	| None ->
		comment ctx "SA-TODO(v0.19): unresolvable payload type";
		Imm "0"
	| Some ty ->
		let r = fresh ctx "t" in
		emit ctx (Printf.sprintf "%s = load %s+%d as %s" r b ((index + 1) * 8) ty);
		track ctx r;
		Reg r)

(** Constructor payload mem-annotations by tag for an enum decl:
	`[(tag, [mem-ty...])]` sorted by tag. None when any constructor
	carries non-scalar payloads (String/nested → honest TODO) or the
	type is not a concrete enum. *)
and enum_ctor_info t =
	match follow t with
	| TEnum (e, _) ->
		let ctors = PMap.fold (fun cf acc -> cf :: acc) e.e_constrs [] in
		let info = List.map (fun cf ->
			match follow cf.ef_type with
			| TFun (params, _) ->
				let tys = List.map (fun (_, _, pt) ->
					match follow pt with
					| TAbstract ({ a_path = ([], "Int") }, _)
					| TAbstract ({ a_path = ([], "Bool") }, _) -> Some "i32"
					| TAbstract ({ a_path = ([], "Float") }, _) -> Some "f64"
					| _ -> None
				) params in
				if List.exists ((=) None) tys then None
				else Some (cf.ef_index,
					List.map (function Some s -> s | None -> "") tys)
			| _ ->
				Some (cf.ef_index, [])
		) ctors in
		if List.exists ((=) None) info then None
		else Some (List.sort compare
			(List.map (function Some x -> x | None -> (0, [])) info))
	| _ -> None

(** Structural enum equality: tags first, then per-constructor payload
	words (scalar only). Merge discipline: res is defined exactly once
	per path (DIFF/ZERO/arms); test/tag temps released on all paths. *)
and gen_enum_eq ctx op e1 e2 =
	let same_decls = match enum_ctor_info e1.etype, enum_ctor_info e2.etype with
		| Some a, Some b when List.map fst a = List.map fst b -> Some a
		| _ -> None in
	match same_decls with
	| None ->
		comment ctx "SA-TODO(v0.21): enum eq needs scalar payloads";
		Imm "0"
	| Some infos ->
	match gen_operand ctx e1, gen_operand ctx e2 with
	| Reg a, Reg b ->
		let snap_all = snapshot ctx in
		let ta = fresh ctx "t" in
		emit ctx (Printf.sprintf "%s = load %s+0 as i32" ta a);
		track ctx ta;
		let tb = fresh ctx "t" in
		emit ctx (Printf.sprintf "%s = load %s+0 as i32" tb b);
		track ctx tb;
		let teq = fresh ctx "t" in
		emit ctx (Printf.sprintf "%s = eq %s, %s" teq ta tb);
		track ctx teq;
		let res = fresh ctx "t" in
		let et = fresh ctx "t" in
		track ctx et;
		let l_same = fresh_label ctx "EQSAME" in
		let l_diff = fresh_label ctx "EQDIFF" in
		let l_end = fresh_label ctx "EQEND" in
		let first_test = ref true in
		emit ctx (Printf.sprintf "br %s -> %s, %s" teq l_same l_diff);
		emit_label ctx l_diff;
		emit ctx (Printf.sprintf "%s = add 0, 0" res);
		let saved_d = save_live ctx in
		release_now ctx ta;
		release_now ctx tb;
		release_now ctx teq;
		restore_live ctx saved_d;
		emit ctx (Printf.sprintf "jmp %s" l_end);
		emit_label ctx l_same;
		let saved_tests = save_live ctx in
		let emit_arm_body ptys =
			let accr = ref "" in
			List.iteri (fun i ty ->
				let pa = fresh ctx "t" in
				emit ctx (Printf.sprintf "%s = load %s+%d as %s" pa a ((i + 1) * 8) ty);
				track ctx pa;
				let pb = fresh ctx "t" in
				emit ctx (Printf.sprintf "%s = load %s+%d as %s" pb b ((i + 1) * 8) ty);
				track ctx pb;
				let mn = if ty = "f64" then "fcmp_eq" else "eq" in
				let c = fresh ctx "t" in
				emit ctx (Printf.sprintf "%s = %s %s, %s" c mn pa pb);
				track ctx c;
				if !accr = "" then accr := c
				else begin
					let acc2 = fresh ctx "t" in
					emit ctx (Printf.sprintf "%s = and %s, %s" acc2 !accr c);
					track ctx acc2;
					release_now ctx !accr;
					release_now ctx c;
					accr := acc2
				end;
				release_now ctx pa;
				release_now ctx pb
			) ptys;
			if !accr = "" then emit ctx (Printf.sprintf "%s = add 1, 0" res)
			else begin
				emit ctx (Printf.sprintf "%s = add %s, 0" res !accr);
				release_now ctx !accr
			end;
			emit ctx (Printf.sprintf "jmp %s" l_end) in
		let rec arms = function
			| [] -> ()
			| [(tag, ptys)] ->
				if not !first_test then emit ctx ("!" ^ et);
				first_test := false;
				emit ctx (Printf.sprintf "%s = eq %s, %d" et ta tag);
				let l_arm = fresh_label ctx "EQARM" in
				let l_zero = fresh_label ctx "EQZERO" in
				emit ctx (Printf.sprintf "br %s -> %s, %s" et l_arm l_zero);
				emit_label ctx l_arm;
				restore_live ctx saved_tests;
				release_since ctx snap_all;
				emit_arm_body ptys;
				emit_label ctx l_zero;
				restore_live ctx saved_tests;
				release_since ctx snap_all;
				emit ctx (Printf.sprintf "%s = add 0, 0" res);
				emit ctx (Printf.sprintf "jmp %s" l_end)
			| (tag, ptys) :: rest ->
				if not !first_test then emit ctx ("!" ^ et);
				first_test := false;
				emit ctx (Printf.sprintf "%s = eq %s, %d" et ta tag);
				let l_arm = fresh_label ctx "EQARM" in
				let l_next = fresh_label ctx "EQNEXT" in
				emit ctx (Printf.sprintf "br %s -> %s, %s" et l_arm l_next);
				emit_label ctx l_arm;
				restore_live ctx saved_tests;
				release_since ctx snap_all;
				emit_arm_body ptys;
				emit_label ctx l_next;
				arms rest in
		arms infos;
		emit_label ctx l_end;
		restore_live ctx (keep_oldest snap_all saved_tests);
		track ctx res;
		if op = OpNotEq then begin
			let n = fresh ctx "t" in
			emit ctx (Printf.sprintf "%s = eq %s, 0" n res);
			track ctx n;
			Reg n
		end else Reg res
	| _ ->
		comment ctx "SA-TODO(v0.21): enum base must be registers";
		Imm "0"

and gen_switch ctx sw =
	let all_pats = List.concat (List.map (fun c -> c.case_patterns) sw.switch_cases) in
	let str_mode = is_string_t sw.switch_subject.etype in
	let str_pats = List.map (fun p -> match p.eexpr with
		| TConst (TString s) ->
			let (name, len) = intern_string ctx s in
			Some ("&" ^ name, string_of_int len)
		| TParenthesis e1 | TMeta (_, e1) | TCast (e1, _) -> begin
			match e1.eexpr with
			| TConst (TString s) ->
				let (name, len) = intern_string ctx s in
				Some ("&" ^ name, string_of_int len)
			| _ -> None
		end
		| _ -> None) all_pats in
	if str_mode && List.exists ((=) None) str_pats then begin
		comment ctx "SA-TODO(v0.6): exotic string switch patterns";
		false
	end else if (not str_mode)
		&& List.exists (fun p -> switch_pat_const p = None) all_pats then begin
		comment ctx "SA-TODO(v0.6): exotic switch patterns (guard/payload)";
		false
	end else begin
		let snap_all = snapshot ctx in
		let subj_str = if str_mode then string_pair_or_call ctx sw.switch_subject else None in
		if str_mode && subj_str = None then begin
			comment ctx "SA-TODO(v0.6): string switch needs tracked lengths";
			false
		end else begin
		let ss = if str_mode then "" else
			match gen_operand ctx sw.switch_subject with
			| Imm s -> s | Reg r -> r in
		let ssubj = match subj_str with Some (p, l) -> (p, ops l) | None -> ("", "") in
		let l_end = fresh_label ctx "ENDSWITCH" in
		let l_def = match sw.switch_default with
			| Some _ -> Some (fresh_label ctx "SWDEF")
			| None -> None in
		let tag = ctx.next_label in
		ctx.next_label <- ctx.next_label + 1;
		let arms = List.mapi (fun i c ->
			(Printf.sprintf "L_ARM%d_%d" tag i, c)) sw.switch_cases in
		(* No-default no-match edge would carry test temps into the
			merge while arms release them: route it through a
			trampoline that normalizes to snap_all first. *)
		let l_nomatch = match sw.switch_default, arms with
			| None, _ :: _ -> Some (fresh_label ctx "SWNOMATCH")
			| _ -> None in
		let str_pat_list = List.map (function Some x -> x | None -> ("", "")) str_pats in
		let str_idx = ref str_pat_list in
		(* Single reusable test register: redefined per test after an
			explicit release, so every merge edge carries the same live
			set (no per-test temp pileup). *)
		let swt = fresh ctx "swt" in
		track ctx swt;
		let first_test = ref true in
		let reuse_test emit_body =
			if not !first_test then emit ctx ("!" ^ swt);
			first_test := false;
			emit_body swt in
		let next_str_pats n =
			let rec take k acc l = match k, l with
				| 0, _ -> (List.rev acc, l)
				| _, [] -> (List.rev acc, [])
				| _, x :: xs -> take (k - 1) (x :: acc) xs in
			let (a, b) = take n [] !str_idx in
			str_idx := b; a in
		let rec emit_tests = function
			| [] -> ()
			| (arm_label, c) :: rest ->
				let ft = match rest, l_def, l_nomatch with
					| [], None, Some nm -> nm
					| [], None, None -> l_end
					| [], Some d, _ -> d
					| _ -> fresh_label ctx "SWNEXT" in
				if c.case_patterns = [] then
					emit ctx (Printf.sprintf "jmp %s" arm_label)
				else if str_mode then begin
					let sps = next_str_pats (List.length c.case_patterns) in
					let rec one_spat = function
						| [] -> ()
						| [(pp, lp)] ->
							let (ps, ls) = ssubj in
							reuse_test (fun t ->
								emit ctx (Printf.sprintf "EXPAND STRING_EQ %s, %s, %s, %s, %s"
									t ps ls pp lp));
							emit ctx (Printf.sprintf "br %s -> %s, %s" swt arm_label ft);
							let is_fall = match l_def with
								| Some d -> ft <> l_end && ft <> d
								| None -> ft <> l_end in
							if is_fall then begin
								emit_label ctx ft;
								(match l_nomatch with
								| Some nm when nm = ft ->
									let saved_t = save_live ctx in
									release_since ctx snap_all;
									emit ctx (Printf.sprintf "jmp %s" l_end);
									restore_live ctx saved_t
								| _ -> ())
							end
						| (pp, lp) :: sps ->
							let (ps, ls) = ssubj in
							reuse_test (fun t ->
								emit ctx (Printf.sprintf "EXPAND STRING_EQ %s, %s, %s, %s, %s"
									t ps ls pp lp));
							let l_or = fresh_label ctx "SWOR" in
							emit ctx (Printf.sprintf "br %s -> %s, %s" swt arm_label l_or);
							emit_label ctx l_or;
							one_spat sps
					in
					one_spat sps;
					emit_tests rest
				end else begin
				let rec one_pat = function
					| [] -> ()
					| [p] ->
						let ps = match switch_pat_const p with
							| Some s -> s | None -> "0" in
						reuse_test (fun t ->
							emit ctx (Printf.sprintf "%s = eq %s, %s" t ss ps));
						emit ctx (Printf.sprintf "br %s -> %s, %s" swt arm_label ft);
						let is_fall = match l_def with
							| Some d -> ft <> l_end && ft <> d
							| None -> ft <> l_end in
						if is_fall then begin
							emit_label ctx ft;
							(match l_nomatch with
							| Some nm when nm = ft ->
								let saved_t = save_live ctx in
								release_since ctx snap_all;
								emit ctx (Printf.sprintf "jmp %s" l_end);
								restore_live ctx saved_t
							| _ -> ())
						end
					| p :: ps ->
						let pcs = match switch_pat_const p with
							| Some s -> s | None -> "0" in
						reuse_test (fun t ->
							emit ctx (Printf.sprintf "%s = eq %s, %s" t ss pcs));
						let l_or = fresh_label ctx "SWOR" in
						emit ctx (Printf.sprintf "br %s -> %s, %s" swt arm_label l_or);
						emit_label ctx l_or;
						one_pat ps
				in
				one_pat c.case_patterns;
				emit_tests rest
				end
		in
		emit_tests arms;
		let saved_full = save_live ctx in
		let all_term = ref true in
		List.iter (fun (arm_label, c) ->
			emit_label ctx arm_label;
			restore_live ctx saved_full;
			let term = gen_stmt ctx c.case_expr in
			release_since ctx snap_all;
			if not term then emit ctx (Printf.sprintf "jmp %s" l_end);
			all_term := !all_term && term
		) arms;
		(match sw.switch_default, l_def with
		| Some d, Some ld ->
			emit_label ctx ld;
			restore_live ctx saved_full;
			let term = gen_stmt ctx d in
			release_since ctx snap_all;
			if not term then emit ctx (Printf.sprintf "jmp %s" l_end);
			all_term := !all_term && term
		| _ -> ());
		(* A nomatch trampoline always falls through to the merge, so a
			switch without default never counts as terminated. *)
		if l_nomatch <> None then all_term := false;
		if arms = [] && sw.switch_default = None then all_term := false;
		if not !all_term then emit_label ctx l_end;
		if arms <> [] || sw.switch_default <> None then
			restore_live ctx (keep_oldest snap_all saved_full);
		!all_term
		end
	end

(* Joins the gen_operand/gen_switch `rec` chain above: switch arms,
	if branches and loop bodies lower statements, statements contain
	expressions and nested control flow. *)
and gen_stmt ctx e =
	match e.eexpr with
	| TBlock el ->
		let term = ref false in
		List.iter (fun s ->
			if not !term then term := gen_stmt ctx s) el;
		!term
	| TVar (v, init) ->
		let slot =
			try Hashtbl.find ctx.vars v.v_id
			with Not_found ->
				let s = fresh ctx "var" in
				Hashtbl.replace ctx.vars v.v_id s;
				emit ctx (Printf.sprintf "%s = stack_alloc 8" s);
				s
		in
		if is_string_t v.v_type then begin
			(match init with
			| Some ie -> (match materialize_owned ctx ie with
				| Some (p, lop) ->
					emit ctx (Printf.sprintf "store %s+0, %s as ptr" slot p);
					Hashtbl.replace ctx.str_lens v.v_id lop;
					false
				| None ->
					comment ctx "SA-TODO(v0.14): unmaterializable string init";
					emit ctx (Printf.sprintf "store %s+0, 0 as ptr" slot);
					false)
			| None ->
				emit ctx (Printf.sprintf "store %s+0, 0 as ptr" slot);
				false)
		end else begin
		let ty = mem_ty v.v_type in
		let valu = match init with
			| Some ie -> begin match gen_operand ctx ie with
				| Imm s -> s | Reg r -> r end
			| None -> "0"
		in
		emit ctx (Printf.sprintf "store %s+0, %s as %s" slot valu ty);
		track_str_len ctx v init;
		false
		end
	| TReturn ret ->
		let keep = match ret with
			| Some re when is_string_t re.etype && ctx.ret_kind = "String" ->
				begin match materialize_owned ctx re with
				| Some (p, lop) ->
					let ls = match lop with Imm s -> s | Reg rr -> rr in
					emit ctx (Printf.sprintf "store __retlen+0, %s as u64" ls);
					let keep = if String.length p > 0 && p.[0] = '&' then None
						else Some p in
					release_all_except ctx keep;
					Imm p
				| None ->
					comment ctx "SA-TODO(v0.20): unreturnable string";
					emit ctx "store __retlen+0, 0 as u64";
					release_all_except ctx None; Imm "0"
				end
			| Some re -> begin match gen_operand ctx re with
				| Imm s -> release_all_except ctx None; Imm s
				| Reg r -> release_all_except ctx (Some r); Reg r end
			| None -> release_all_except ctx None; Imm "0"
		in
		let rs = match keep with Imm s -> s | Reg r -> r in
		emit ctx (Printf.sprintf "return %s" rs);
		true
	| TCall (fn, args) when callee_is_trace fn ->
		gen_trace ctx args;
		false
	| TCall ({ eexpr = TField (_, FStatic (c, cf)) }, [lnr])
		when s_type_path c.cl_path = "sa.net.Tcp" && cf.cf_name = "close" ->
		(match gen_operand ctx lnr with
		| Imm s -> ignore (net_close_stmt ctx s); false
		| Reg r -> ignore (net_close_stmt ctx r); false)
	| TCall ({ eexpr = TField (_, FStatic (c, cf)) }, [s; ms])
		when (s_type_path c.cl_path = "sa.net.Tcp"
			|| s_type_path c.cl_path = "sa.net.Udp")
			&& cf.cf_name = "setReadTimeout" ->
		ignore (net_set_timeout_stmt ctx s ms
			(s_type_path c.cl_path = "sa.net.Udp")); false
	| TCall ({ eexpr = TField (_, FStatic (c, cf)) }, [s])
		when s_type_path c.cl_path = "sa.net.Tcp" && cf.cf_name = "closeStream" ->
		ignore (net_stream_close_stmt ctx s false); false
	| TCall ({ eexpr = TField (_, FStatic (c, cf)) }, [s])
		when s_type_path c.cl_path = "sa.net.Udp" && cf.cf_name = "close" ->
		ignore (net_stream_close_stmt ctx s true); false
	| TCall ({ eexpr = TField (_, FStatic (c, cf)) }, [s; host; port])
		when s_type_path c.cl_path = "sa.net.Udp" && cf.cf_name = "connect" ->
		(match gen_operand ctx s with
		| Imm _ ->
			comment ctx "SA-TODO(v0.23): udp connect needs socket register";
			false
		| Reg r -> ignore (net_udp_connect_stmt ctx r host port); false)
	| TCall ({ eexpr = TField (_, FStatic (c, cf)) }, args) ->
		if sys_surface_stmt ctx c cf args then false
		else (match s_type_path c.cl_path, cf.cf_name, args with
		| "Sys", "putEnv", [k; v] ->
			ignore (sys_putenv ctx k v); false
		| "Sys", "sleep", [s] ->
			ignore (sys_sleep ctx s); false
		| _ -> ignore (gen_call ctx c cf args); false)
	| TCall ({ eexpr = TField (obj, FInstance (c, _, cf)) }, args) ->
		ignore (gen_method_call ctx c cf obj args);
		false
	| TCall (fn, _) ->
		comment ctx ("SA-TODO(v0.4): general call (" ^ callee_kind fn ^ ")");
		false
	| TBinop (OpAssign, _, _) ->
		ignore (gen_operand ctx e);
		false
	| TIf (cond, then_e, else_opt) ->
		let cs = cond_reg ctx cond in
		let l_then = fresh_label ctx "THEN" in
		let l_end = fresh_label ctx "ENDIF" in
		let arm_state = save_live ctx in
		(match else_opt with
		| Some else_e ->
			let l_else = fresh_label ctx "ELSE" in
			emit ctx (Printf.sprintf "br %s -> %s, %s" cs l_then l_else);
			let snap = snapshot ctx in
			emit_label ctx l_then;
			restore_live ctx arm_state;
			let term_then = gen_stmt ctx then_e in
			release_since ctx snap;
			if not term_then then emit ctx (Printf.sprintf "jmp %s" l_end);
			emit_label ctx l_else;
			restore_live ctx arm_state;
			let term_else = gen_stmt ctx else_e in
			release_since ctx snap;
			if not term_else then emit ctx (Printf.sprintf "jmp %s" l_end);
			(* Fully terminated: no edge reaches the merge; emitting
				an empty trailing label trips FallthroughForbidden. *)
			if not (term_then && term_else) then emit_label ctx l_end;
			restore_live ctx arm_state;
			term_then && term_else
		| None ->
			emit ctx (Printf.sprintf "br %s -> %s, %s" cs l_then l_end);
			let snap = snapshot ctx in
			emit_label ctx l_then;
			restore_live ctx arm_state;
			let term_then = gen_stmt ctx then_e in
			release_since ctx snap;
			if not term_then then emit ctx (Printf.sprintf "jmp %s" l_end);
			emit_label ctx l_end;
			restore_live ctx arm_state;
			false)
	| TWhile (cond, body, flag) ->
		let l_cond = fresh_label ctx "COND" in
		let l_body = fresh_label ctx "BODY" in
		let l_end = fresh_label ctx "ENDWHILE" in
		let entry_snap = snapshot ctx in
		let saved_pre = save_live ctx in
		let cond_snap = ref entry_snap in
		let cond_regs = ref [] in
		ctx.loops <- (l_end, l_cond, entry_snap, cond_snap) :: ctx.loops;
		let emit_cond () =
			emit_label ctx l_cond;
			let n0 = List.length ctx.live in
			let cs = cond_reg ctx cond in
			emit ctx (Printf.sprintf "br %s -> %s, %s" cs l_body l_end);
			cond_snap := snapshot ctx;
			cond_regs := take_live (List.length ctx.live - n0) ctx.live
		in
		let emit_body () =
			emit_label ctx l_body;
			(* No release here: cond temps stay live so that `break`
				edges match the cond-false edge at the loop end.
				Bottom/continue release everything to entry_snap. *)
			let term = gen_stmt ctx body in
			release_since ctx entry_snap;
			if not term then emit ctx (Printf.sprintf "jmp %s" l_cond)
		in
		(match flag with
		| NormalWhile ->
			emit ctx (Printf.sprintf "jmp %s" l_cond);
			emit_cond ();
			emit_body ()
		| DoWhile when has_direct_jump 0 body ->
			comment ctx "SA-TODO(v0.3): do-while with break/continue";
			release_since ctx entry_snap
		| DoWhile ->
			emit_body ();
			emit_cond ());
		ctx.loops <- (match ctx.loops with _ :: rest -> rest | [] -> []);
		emit_label ctx l_end;
		(* Post-loop runtime live = pre-loop state + cond temps. *)
		restore_live ctx (!cond_regs @ keep_oldest entry_snap saved_pre);
		false
	| TBreak -> begin
		match ctx.loops with
		| (l_end, _, _, cond_snap) :: _ ->
			(* Keep cond temps: the cond-false edge carries them too. *)
			release_since ctx !cond_snap;
			emit ctx (Printf.sprintf "jmp %s" l_end);
			true
		| [] -> comment ctx "SA-TODO: break outside loop"; false
	end
	| TContinue -> begin
		match ctx.loops with
		| (_, l_cond, entry_snap, _) :: _ ->
			release_since ctx entry_snap;
			emit ctx (Printf.sprintf "jmp %s" l_cond);
			true
		| [] -> comment ctx "SA-TODO: continue outside loop"; false
	end
	| TSwitch sw ->
		gen_switch ctx sw
	| TThrow _ ->
		(* Uncaught throw aborts like panic (payload dropped, noted).
			Caught throws need landing pads: v0.16 only lowers the
			abort shape; try_may_abort guards try bodies. *)
		comment ctx "SA-NOTE: throw payload dropped, abort preserved";
		emit ctx "panic(\"haxe:throw\")";
		true
	| TTry (body, _) ->
		if try_may_abort body then begin
			comment ctx "SA-TODO(v0.16): try over aborting body";
			false
		end else begin
			comment ctx "SA-NOTE: catch elided (body cannot abort)";
			gen_stmt ctx body
		end
	| TParenthesis e1 | TMeta (_, e1) -> gen_stmt ctx e1
	| TConst _ | TLocal _ | TBinop _ | TUnop _ | TArray _ | TArrayDecl _
	| TField _ | TObjectDecl _ ->
		ignore (gen_operand ctx e);
		false
	| _ ->
		comment ctx ("SA-TODO: unsupported statement " ^ expr_kind e);
		false

and gen_function fctx name tf exps ret body_stmts this_kind =
	ignore (ensure_pslot fctx);
	fctx.ret_kind <- ret;
	if ret = "String" then track fctx "__retlen";
	(* `this` homing: constructors allocate, methods home the self
		param. The slot makes field access uniform with locals. *)
	let this_ret = match this_kind with
		| `ThisAlloc size ->
			let obj = fresh fctx "obj" in
			emit fctx (Printf.sprintf "%s = alloc %d" obj size);
			track fctx obj;
			let slot = fresh fctx "var" in
			emit fctx (Printf.sprintf "%s = stack_alloc 8" slot);
			emit fctx (Printf.sprintf "store %s+0, %s as ptr" slot obj);
			fctx.this_slot <- Some slot;
			Some obj
		| `ThisParam ->
			let slot = fresh fctx "var" in
			emit fctx (Printf.sprintf "%s = stack_alloc 8" slot);
			emit fctx (Printf.sprintf "store %s+0, self as ptr" slot);
			track fctx "self";
			fctx.this_slot <- Some slot;
			None
		| `NoThis -> None in
	(* Parameter homing: scalars copy by value; String pairs home the
		data pointer and record the live length. *)
	List.iter (function
		| Scalar (v, ty) ->
			let slot = fresh fctx "var" in
			Hashtbl.replace fctx.vars v.v_id slot;
			emit fctx (Printf.sprintf "%s = stack_alloc 8" slot);
			emit fctx (Printf.sprintf "store %s+0, %s as %s" slot v.v_name ty);
			(* Params are live registers too: homing copies them, the
				originals must still be released on every exit. *)
			track fctx v.v_name
		| StrPair v ->
			let slot = fresh fctx "var" in
			Hashtbl.replace fctx.vars v.v_id slot;
			emit fctx (Printf.sprintf "%s = stack_alloc 8" slot);
			emit fctx (Printf.sprintf "store %s+0, %s as ptr" slot (str_ptr_name v));
			Hashtbl.replace fctx.str_lens v.v_id (Reg (str_len_name v));
			track fctx (str_ptr_name v);
			track fctx (str_len_name v)
	) exps;
	let hoist = List.concat (List.map (collect_vars []) body_stmts) in
	List.iter (fun v ->
		if not (Hashtbl.mem fctx.vars v.v_id) then begin
			let slot = fresh fctx "var" in
			Hashtbl.replace fctx.vars v.v_id slot;
			emit fctx (Printf.sprintf "%s = stack_alloc 8" slot)
		end
	) hoist;
	let term = ref false in
	List.iter (fun s -> term := gen_stmt fctx s) body_stmts;
	if not !term then begin
		match this_ret with
		| Some obj ->
			release_all_except fctx (Some obj);
			emit fctx (Printf.sprintf "return %s" obj)
		| None ->
			release_all_except fctx None;
			(match ret with
			| "void" -> emit fctx "return"
			| "String" ->
				emit fctx "store __retlen+0, 0 as u64";
				emit fctx "return 0"
			| _ -> emit fctx "return 0")
	end;
	let b = Buffer.create 2048 in
	let params = List.concat (List.map (function
		| Scalar (v, ty) -> [Printf.sprintf "%s: %s" v.v_name ty]
		| StrPair v -> [Printf.sprintf "%s: ptr" (str_ptr_name v);
			Printf.sprintf "%s: u64" (str_len_name v)]
	) exps) in
	let params = match this_kind with
		| `ThisParam -> "self: ptr" :: params
		| _ -> params in
	let ret_ty = if ret = "String" then "ptr" else ret in
	let params = if ret = "String" then params @ ["__retlen: ptr"] else params in
	let sig_ = String.concat ", " params in
	Buffer.add_string b (Printf.sprintf "
@%s(%s) -> %s:
L_ENTRY:
" name sig_ ret_ty);
	Buffer.add_string b (Buffer.contents fctx.buf);
	Buffer.add_string fctx.funcs (Buffer.contents b)



(** Lower a static call to an emitted helper. Arguments evaluate
	left-to-right; void calls emit bare `call`, value calls `r = call`.
	String formals splice to `(ptr, len)` pairs via string_operands. *)
and splice_args ctx cf args : string list option =
	let formals = match follow cf.cf_type with
		| TFun (f, _) -> List.map (fun (_, _, t) -> t) f
		| _ -> [] in
	let rec loop acc = function
		| [], [] -> Some (List.rev acc)
		| a :: rest_a, t :: rest_t ->
			if is_string_t t then
				(match string_pair_or_call ctx a with
				| Some (p, l) -> loop (ops l :: p :: acc) (rest_a, rest_t)
				| None -> None)
			else begin
				let s = match gen_operand ctx a with
					| Imm s -> s | Reg r -> r in
				loop (s :: acc) (rest_a, rest_t)
			end
		| _ -> None in
	loop [] (args, formals)

(** v0.18 TCP surface over existing `NET_TCP_*` macros (zero new ABI).
	Handles stay u64-typed regs (Haxe `UInt` slots); failures panic
	loudly like fs. Sockets/streams stay deferred. *)
and panic_unless_ok ctx st msg =
	let ok = fresh ctx "t" in
	emit ctx (Printf.sprintf "%s = eq %s, 0" ok st);
	track ctx ok;
	let l_ok = fresh_label ctx "NETOK" in
	let l_fail = fresh_label ctx "NETFAIL" in
	emit ctx (Printf.sprintf "br %s -> %s, %s" ok l_ok l_fail);
	emit_label ctx l_fail;
	emit ctx (Printf.sprintf "panic(\"%s\")" msg);
	emit_label ctx l_ok;
	release_now ctx ok

and net_listen_op ctx port =
	let ps = match port with Imm s -> s | Reg r -> r in
	let (hn, hl) = intern_string ctx "127.0.0.1" in
	let st = fresh ctx "t" in
	let lnr = fresh ctx "t" in
	let dp = fresh ctx "t" in
	emit ctx (Printf.sprintf
		"EXPAND NET_TCP_LISTENER_BIND_PORT %s, %s, %s, &%s, %d, %s"
		st lnr dp hn hl ps);
	track ctx st;
	track ctx lnr;
	track ctx dp;
	panic_unless_ok ctx st "haxe:net-listen";
	release_now ctx st;
	release_now ctx dp;
	Reg lnr

and net_bound_port_op ctx lnr =
	let st = fresh ctx "t" in
	let addr = fresh ctx "t" in
	emit ctx (Printf.sprintf "EXPAND NET_TCP_LISTENER_LOCAL_ADDR %s, %s, %s"
		st addr lnr);
	track ctx st;
	track ctx addr;
	panic_unless_ok ctx st "haxe:net-local-addr";
	release_now ctx st;
	let port = fresh ctx "t" in
	emit ctx (Printf.sprintf "EXPAND NET_ADDR_PORT %s, %s" port addr);
	track ctx port;
	let st2 = fresh ctx "t" in
	emit ctx (Printf.sprintf "EXPAND NET_ADDR_FREE %s, %s" st2 addr);
	track ctx st2;
	release_now ctx st2;
	release_now ctx addr;
	Reg port

and net_close_stmt ctx lnr =
	let st = fresh ctx "t" in
	emit ctx (Printf.sprintf "EXPAND NET_TCP_LISTENER_CLOSE %s, %s" st lnr);
	track ctx st;
	panic_unless_ok ctx st "haxe:net-close";
	release_now ctx st;
	true

(** v0.23 TCP/UDP stream surface over existing `NET_TCP_*` / `NET_UDP_*`
	macros (zero new ABI). Status-checked calls panic loudly; u64
	handles flow through `UInt` regs. String results use the
	`__retlen` convention via materialize. *)
and net_tcp_connect_op ctx host_e port_e =
	match string_operands ctx host_e with
	| Some (hp, hl) ->
		let ps = match gen_operand ctx port_e with
			| Imm s -> s | Reg r -> r in
		let st = fresh ctx "t" in
		let s = fresh ctx "t" in
		emit ctx (Printf.sprintf "EXPAND NET_TCP_CONNECT %s, %s, %s, %s, %s"
			st s hp hl ps);
		track ctx st;
		track ctx s;
		panic_unless_ok ctx st "haxe:net-connect";
		release_now ctx st;
		Reg s
	| None ->
		comment ctx "SA-TODO(v0.23): connect needs resolvable host";
		Imm "0"

and net_tcp_write_op ctx stream_e data_e =
	match gen_operand ctx stream_e with
	| Imm _ ->
		comment ctx "SA-TODO(v0.23): write needs stream register";
		Imm "0"
	| Reg ss ->
	match string_operands ctx data_e with
	| Some (dp, dl) ->
		let st = fresh ctx "t" in
		let n = fresh ctx "t" in
		emit ctx (Printf.sprintf "EXPAND NET_TCP_STREAM_WRITE %s, %s, %s, %s, %s"
			st n ss dp dl);
		track ctx st;
		track ctx n;
		panic_unless_ok ctx st "haxe:net-write";
		release_now ctx st;
		Reg n
	| None ->
		comment ctx "SA-TODO(v0.23): write needs resolvable data";
		Imm "0"

and net_set_timeout_stmt ctx stream_e ms_e is_udp =
	match gen_operand ctx stream_e with
	| Imm _ ->
		comment ctx "SA-TODO(v0.23): timeout needs stream register";
		false
	| Reg ss ->
	let vs = match gen_operand ctx ms_e with
		| Imm s -> s | Reg r -> r in
	let ns = fresh ctx "t" in
	emit ctx (Printf.sprintf "%s = mul %s, 1000000" ns vs);
	track ctx ns;
	let st = fresh ctx "t" in
	let mn = if is_udp then "NET_UDP_SET_READ_TIMEOUT" else "NET_TCP_STREAM_SET_READ_TIMEOUT" in
	emit ctx (Printf.sprintf "EXPAND %s %s, %s, %s" mn st ss ns);
	track ctx st;
	panic_unless_ok ctx st "haxe:net-timeout";
	release_now ctx st;
	release_now ctx ns;
	true

and net_stream_close_stmt ctx stream_e is_udp =
	match gen_operand ctx stream_e with
	| Imm _ ->
		comment ctx "SA-TODO(v0.23): close needs stream register";
		false
	| Reg ss ->
	let st = fresh ctx "t" in
	let mn = if is_udp then "NET_UDP_CLOSE" else "NET_TCP_STREAM_CLOSE" in
	emit ctx (Printf.sprintf "EXPAND %s %s, %s" mn st ss);
	track ctx st;
	panic_unless_ok ctx st "haxe:net-stream-close";
	release_now ctx st;
	true

and net_udp_bind_op ctx port_e =
	let ps = match gen_operand ctx port_e with
		| Imm s -> s | Reg r -> r in
	let (hn, hl) = intern_string ctx "127.0.0.1" in
	let st = fresh ctx "t" in
	let s = fresh ctx "t" in
	emit ctx (Printf.sprintf "EXPAND NET_UDP_BIND %s, %s, &%s, %d, %s"
		st s hn hl ps);
	track ctx st;
	track ctx s;
	panic_unless_ok ctx st "haxe:net-udp-bind";
	release_now ctx st;
	Reg s

and net_udp_port_op ctx s =
	let st = fresh ctx "t" in
	let addr = fresh ctx "t" in
	emit ctx (Printf.sprintf "EXPAND NET_UDP_LOCAL_ADDR %s, %s, %s" st addr s);
	track ctx st;
	track ctx addr;
	panic_unless_ok ctx st "haxe:net-udp-addr";
	release_now ctx st;
	let port = fresh ctx "t" in
	emit ctx (Printf.sprintf "EXPAND NET_ADDR_PORT %s, %s" port addr);
	track ctx port;
	let st2 = fresh ctx "t" in
	emit ctx (Printf.sprintf "EXPAND NET_ADDR_FREE %s, %s" st2 addr);
	track ctx st2;
	release_now ctx st2;
	release_now ctx addr;
	Reg port

and net_udp_connect_stmt ctx s host_e port_e =
	match string_operands ctx host_e with
	| Some (hp, hl) ->
		let ps = match gen_operand ctx port_e with
			| Imm x -> x | Reg r -> r in
		let st = fresh ctx "t" in
		emit ctx (Printf.sprintf "EXPAND NET_UDP_CONNECT %s, %s, %s, %s, %s"
			st s hp hl ps);
		track ctx st;
		panic_unless_ok ctx st "haxe:net-udp-connect";
		release_now ctx st;
		true
	| None ->
		comment ctx "SA-TODO(v0.23): udp connect needs resolvable host";
		false

and net_udp_send_op ctx s_e data_e =
	match gen_operand ctx s_e with
	| Imm _ ->
		comment ctx "SA-TODO(v0.23): udp send needs socket register";
		Imm "0"
	| Reg s ->
	match string_operands ctx data_e with
	| Some (dp, dl) ->
		let st = fresh ctx "t" in
		let n = fresh ctx "t" in
		emit ctx (Printf.sprintf "EXPAND NET_UDP_SEND %s, %s, %s, %s, %s"
			st n s dp dl);
		track ctx st;
		track ctx n;
		panic_unless_ok ctx st "haxe:net-udp-send";
		release_now ctx st;
		Reg n
	| None ->
		comment ctx "SA-TODO(v0.23): udp send needs resolvable data";
		Imm "0"

(** v0.25 `Date` surface: `std/Date.hx` is extern (no bodies), so
	methods lower directly to `sa_time_*` contracts. Date objects are
	heap `{t:f64 millis}` (field +0), built by `now()`. *)
and date_now_op ctx =
	let m = fresh ctx "t" in
	emit ctx (Printf.sprintf "%s = call @sa_time_unix_ms()" m);
	track ctx m;
	let f = fresh ctx "t" in
	emit ctx (Printf.sprintf "%s = sitofp %s" f m);
	track ctx f;
	let obj = fresh ctx "obj" in
	emit ctx (Printf.sprintf "%s = alloc %d" obj 8);
	track ctx obj;
	emit ctx (Printf.sprintf "store %s+0, %s as f64" obj f);
	release_now ctx m;
	release_now ctx f;
	Reg obj

and date_field_ms ctx obj =
	match gen_operand ctx obj with
	| Imm _ ->
		comment ctx "SA-TODO(v0.25): Date base must be a register";
		None
	| Reg b ->
		let t = fresh ctx "t" in
		emit ctx (Printf.sprintf "%s = load %s+0 as f64" t b);
		track ctx t;
		let m = fresh ctx "t" in
		emit ctx (Printf.sprintf "%s = fptosi %s" m t);
		track ctx m;
		release_now ctx t;
		Some m

and date_getter_op ctx obj sai =
	match date_field_ms ctx obj with
	| None -> Imm "0"
	| Some m ->
		let r = fresh ctx "t" in
		emit ctx (Printf.sprintf "%s = call @%s(%s)" r sai m);
		track ctx r;
		release_now ctx m;
		Reg r

and date_method_op ctx obj cf =
	match cf.cf_name with
	| "getTime" ->
		(match gen_operand ctx obj with
		| Imm _ ->
			comment ctx "SA-TODO(v0.25): Date base must be a register";
			Imm "0"
		| Reg b ->
			let r = fresh ctx "t" in
			emit ctx (Printf.sprintf "%s = load %s+0 as f64" r b);
			track ctx r;
			Reg r)
	| ("getFullYear" | "getMonth" | "getDate" | "getHours" | "getMinutes" | "getSeconds" as g) ->
		let sai = match g with
			| "getFullYear" -> "sa_time_get_full_year"
			| "getMonth" -> "sa_time_get_month"
			| "getDate" -> "sa_time_get_date"
			| "getHours" -> "sa_time_get_hours"
			| "getMinutes" -> "sa_time_get_minutes"
			| _ -> "sa_time_get_seconds" in
		date_getter_op ctx obj sai
	| _ ->
		comment ctx ("SA-TODO(v0.25): Date." ^ cf.cf_name);
		Imm "0"

and gen_call ctx c cf args =
	let name = sa_fun_name c cf in
	if not (Hashtbl.mem ctx.emitted name) then begin
		if cf_is_bare_generic cf then generic_hint ctx c cf
		else comment ctx ("SA-TODO(v0.6): call to non-emitted " ^
			s_type_path c.cl_path ^ "." ^ cf.cf_name);
		Imm "0"
	end else match splice_args ctx cf args with
	| None ->
		comment ctx "SA-TODO(v0.13): unresolvable call argument";
		Imm "0"
	| Some ss -> begin
		let (ret, is_str_ret) = match follow cf.cf_type with
			| TFun (_, r) -> (scalar_ret r, is_string_t r)
			| _ -> (None, false) in
		let ss = if is_str_ret then
			let ps = (match ctx.pslot with Some s -> s | None -> ensure_pslot ctx) in
			ss @ ["&" ^ ps]
		else ss in
		match ret with
		| Some "void" ->
			emit ctx (Printf.sprintf "call @%s(%s)" name (String.concat ", " ss));
			Imm "0"
		| _ ->
			let r = fresh ctx "t" in
			emit ctx (Printf.sprintf "%s = call @%s(%s)" r name (String.concat ", " ss));
			track ctx r;
			Reg r
	end

(** Substring scan: emit `@import "sa_std/string.sa"` iff the output
	actually EXPANDs a string macro (keeps the import graph minimal). *)
let buf_has buf sub =
	let s = Buffer.contents buf in
	let ls = String.length s and lp = String.length sub in
	let rec loop i =
		if i + lp > ls then false
		else String.sub s i lp = sub || loop (i + 1) in
	loop 0

let print_type buf mt =	let c =
		match mt with
		| TClassDecl c -> "// class " ^ (s_type_path c.cl_path)
		| TEnumDecl e -> "// enum " ^ (s_type_path e.e_path)
		| TTypeDecl t -> "// typedef " ^ (s_type_path t.t_path)
		| TAbstractDecl a -> "// abstract " ^ (s_type_path a.a_path)
	in
	Buffer.add_string buf c;
	Buffer.add_char buf '\n'

let generate com =
	let body = Buffer.create 4096 in
	let header = Buffer.create 512 in
	let types = Buffer.create 1024 in
	let funcs = Buffer.create 4096 in
	let emitted : (string, unit) Hashtbl.t = Hashtbl.create 16 in
	let ctx = {
		com; buf = body; header;
		next_reg = 0; next_str = 0; next_label = 0;
		vars = Hashtbl.create 16; live = []; loops = [];
		emitted; funcs;
		str_lens = Hashtbl.create 8; pslot = None; this_slot = None; ret_kind = "i32";
	} in
	List.iter (print_type types) com.types;
	let (stmts, has_args, main_class) = match com.main.main_expr with
		| Some e -> entry_stmts e
		| None -> ([], false, None)
	in
	if has_args then comment ctx "SA-TODO(v0.4): entry with args";
	if stmts = [] then comment ctx "no haxe main entry";
	ignore (ensure_pslot ctx);
	(* Reachability-driven emission: main-class statics (as before)
		plus ctors/methods/statics of classes used by main, closed
		over helper bodies (3 rounds). Pre-registration keeps
		recursion and forward calls resolving. *)
	let wanted_main = List.concat (List.map (collect_classes []) stmts) in
	let is_main_class c = match main_class with
		| Some mc -> mc == c
		| None -> false in
	let helpers = ref [] in
	let register name = Hashtbl.replace emitted name () in
	let done_keys : (string, unit) Hashtbl.t = Hashtbl.create 16 in
	let emit_static c cf =
		let k = "static:" ^ s_type_path c.cl_path ^ "." ^ cf.cf_name in
		if cf.cf_name = "main" || Hashtbl.mem done_keys k then ()
		else match emittable_static cf with
			| None -> ()
			| Some (tf, exps, ret) ->
				Hashtbl.replace done_keys k ();
				let name = sa_fun_name c cf in
				register name;
				helpers := (name, tf, exps, ret, `NoThis) :: !helpers in
	let emit_method c cf =
		let k = "method:" ^ s_type_path c.cl_path ^ "." ^ cf.cf_name in
		if Hashtbl.mem done_keys k then ()
		else match emittable_method cf with
		| None -> ()
		| Some (tf, exps, ret) ->
			Hashtbl.replace done_keys k ();
			let name = sa_fun_name c cf in
			register name;
			helpers := (name, tf, exps, ret, `ThisParam) :: !helpers in
	let emit_ctor c =
		let k = "ctor:" ^ s_type_path c.cl_path in
		if Hashtbl.mem done_keys k then ()
		else begin
			Hashtbl.replace done_keys k ();
			match c.cl_constructor with
			| None -> ()
			| Some cf -> match emittable_ctor c cf with
				| None -> ()
				| Some (tf, exps) ->
					let name = sa_ctor_name c in
					register name;
					helpers := (name, tf, exps, "ptr", `ThisAlloc (class_size c)) :: !helpers
		end in
	(match main_class with
	| None -> ()
	| Some c -> List.iter (fun cf -> emit_static c cf) c.cl_ordered_statics);
	let bodies_of = function
		| (_, tf, _, _, _) -> match tf.tf_expr.eexpr with
			| TBlock el -> el
			| _ -> [tf.tf_expr] in
	let seen_rounds = ref 0 in
	let pending = ref wanted_main in
	let known : (string, unit) Hashtbl.t = Hashtbl.create 16 in
	while !pending <> [] && !seen_rounds < 3 do
		incr seen_rounds;
		let cur = !pending in
		pending := [];
		List.iter (function
			| (_, `Ctor c) -> emit_ctor c
			| (_, `Method (c, cf)) -> emit_method c cf
			| (_, `Static (c, cf)) ->
				if not (is_main_class c) then emit_static c cf
		) cur;
		let fresh_bodies = List.concat (List.map (fun h ->
			List.concat (List.map (collect_classes []) (bodies_of h))
		) !helpers) in
		pending := List.filter (fun (k, _) ->
			not (Hashtbl.mem known k)
		) fresh_bodies;
		List.iter (fun (k, _) -> Hashtbl.replace known k ()) !pending
	done;
	(* Hoist every `stack_alloc` above all branches (PhiStateConflict).
		Stack slots are frame-owned: track nothing (StackEscape). *)
	List.iter (fun v ->
		if not (Hashtbl.mem ctx.vars v.v_id) then begin
			let slot = fresh ctx "var" in
			Hashtbl.replace ctx.vars v.v_id slot;
			emit ctx (Printf.sprintf "%s = stack_alloc 8" slot)
		end
	) (List.concat (List.map (collect_vars []) stmts));
	let main_term = ref false in
	List.iter (fun s -> main_term := gen_stmt ctx s) stmts;
	if not !main_term then begin
		release_all_except ctx None;
		emit ctx "return 0"
	end;
	(* Lower helper bodies with fresh scopes sharing header/emitted. *)
	List.iter (fun (name, tf, exps, ret, this_kind) ->
		let fctx = new_fun_ctx com header emitted funcs in
		let body_stmts = match tf.tf_expr.eexpr with
			| TBlock el -> el
			| _ -> [tf.tf_expr] in
		gen_function fctx name tf exps ret body_stmts this_kind
	) !helpers;
	let ch = open_out_bin com.file in
	output_string ch "// Generated by the Haxe SA target v0.5.\n";
	output_string ch "// + static helpers and calls; see SA_TARGET.md.\n";
	output_string ch "@import \"sa_std/io/print.sai\"\n";
	if buf_has body "STRING_EQ" || buf_has funcs "STRING_EQ"
	|| buf_has body "sa_string_concat" || buf_has funcs "sa_string_concat" then
		output_string ch "@import \"sa_std/string.sa\"\n";
	if buf_has body "sa_fmt_" || buf_has funcs "sa_fmt_" then
		output_string ch "@import \"sa_std/fmt.sai\"\n";
	if buf_has body "sa_time_" || buf_has funcs "sa_time_" then
		output_string ch "@import \"sa_std/time.sai\"\n";
	if buf_has body "sa_math_" || buf_has funcs "sa_math_" then
		output_string ch "@import \"sa_std/math.sai\"\n";
	if buf_has body "NET_TCP_" || buf_has funcs "NET_TCP_"
	|| buf_has body "NET_ADDR_" || buf_has funcs "NET_ADDR_" then
		output_string ch "@import \"sa_std/net.sa\"\n";
	if buf_has body "FS_" || buf_has funcs "FS_"
	|| buf_has body "sa_fs_" || buf_has funcs "sa_fs_" then
		output_string ch "@import \"sa_std/fs.sa\"\n";
	if buf_has body "sa_env_" || buf_has funcs "sa_env_" then
		output_string ch "@import \"sa_std/env.sai\"\n";
	output_string ch "\n";
	output_string ch (Buffer.contents header);
	output_string ch "\n";
	output_string ch (Buffer.contents types);
	output_string ch "\n@main() -> i32:\nL_ENTRY:\n";
	output_string ch (Buffer.contents body);
	output_string ch (Buffer.contents funcs);
	close_out ch