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
	(** Loop stack: (end_label, cond_label, entry_snap, cond_snap).
		`entry_snap` is the live length at loop entry (plain vars);
		`cond_snap` the length after the condition is evaluated
		(cond temps included). All edges into L_COND carry exactly
		`entry_snap` regs; all edges into L_END carry `cond_snap` regs,
		so every merge is Phi-consistent (see sala 06_limitations). *)
	mutable loops : (string * string * int * int ref) list;
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

(** True when the (followed) type is Haxe Float. Everything else is
	treated as i32 for v0.2 (ints, bools as 0/1, chars). *)
let is_float_t t =
	match follow t with
	| TAbstract ({ a_path = ([], "Float") }, _) -> true
	| TInst ({ cl_path = ([], "Float") }, _) -> true
	| _ -> false

(** Memory annotation for homed slots: Float -> f64, Int/Bool -> i32,
	everything else (Array/String/objects) -> ptr. Deterministic per
	type so matching load/store pairs always agree. *)
let mem_ty t =
	if is_float_t t then "f64"
	else match follow t with
	| TAbstract ({ a_path = ([], "Int") }, _)
	| TAbstract ({ a_path = ([], "Bool") }, _) -> "i32"
	| _ -> "ptr"

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

(** Materialize a string literal as `@const STR_n = utf8:"..."` and
	return its name. Byte length = OCaml String.length (UTF-8 bytes). *)
let intern_string ctx s =
	let id = ctx.next_str in
	ctx.next_str <- id + 1;
	let name = Printf.sprintf "STR_%d" id in
	Buffer.add_string ctx.header
		(Printf.sprintf "@const %s = utf8:\"%s\"\n" name (sa_escape s));
	(name, String.length s)

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
			let op = gen_operand ctx rhs in
			let ty = mem_ty v.v_type in
			let os = match op with Imm s -> s | Reg r -> r in
			emit ctx (Printf.sprintf "store %s+0, %s as %s" slot os ty);
			op
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
	| TUnop (op, _, { eexpr = TLocal v }) -> begin
		(* `i++` / `i--` (optimizer also rewrites `i = i + 1`). *)
		try
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
	| TObjectDecl decls -> gen_object_decl ctx decls
	| TField (obj, FAnon cf) -> gen_field_get ctx obj cf.cf_name
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
	| _ ->
		comment ctx ("SA-TODO(v0.2/v0.3): unsupported expression " ^ expr_kind e);
		Imm "0"

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

and gen_binop ctx op e1 e2 etype =
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
		let r = fresh ctx "t" in
		emit ctx (Printf.sprintf "%s = %s %s, %s" r mn s1 s2);
		track ctx r;
		Reg r

(** Resolve the entry body: `main_expr` is usually a `TCall` to the
	static `main` method (or a `TBlock` ending in the `EntryPoint.run`
	call). Returns the statements to lower plus whether the entry
	function takes arguments (v0.2 only handles argless mains). *)
let rec entry_stmts e =
	match e.eexpr with
	| TBlock el ->
		List.fold_left (fun (ss, a) s ->
			let (ss2, a2) = entry_stmts s in (ss @ ss2, a || a2)
		) ([], false) el
	| TMeta (_, e1) | TParenthesis e1 | TCast (e1, _) -> entry_stmts e1
	| TCall ({ eexpr = TField (_, FStatic (_, cf)) }, _) -> begin
		match cf.cf_expr with
		| Some { eexpr = TFunction tf } ->
			let body = match tf.tf_expr.eexpr with
				| TBlock el -> el
				| _ -> [tf.tf_expr]
			in
			(body, tf.tf_args <> [])
		| Some other -> ([other], false)
		| _ -> ([], false)
	end
	| TFunction tf -> ([tf.tf_expr], false)
	| _ -> ([e], false)

(** Callee expression names a `trace`-like function. Covers both the
	global `trace(...)` call and `haxe.Log.trace(...)`. *)
let rec callee_is_trace e =
	match e.eexpr with
	| TIdent "trace" -> true
	| TField (_, FStatic (_, { cf_name = "trace" })) -> true
	| TField (_, FInstance (_, _, { cf_name = "trace" })) -> true
	| TParenthesis e1 | TMeta (_, e1) | TCast (e1, _) -> callee_is_trace e1
	| _ -> false

let callee_kind e =
	match e.eexpr with
	| TIdent s -> "Ident:" ^ s
	| TField (_, FStatic (c, f)) -> "FStatic:" ^ s_type_path c.cl_path ^ "." ^ f.cf_name
	| TField (_, FInstance (c, _, f)) -> "FInstance:" ^ s_type_path c.cl_path ^ "." ^ f.cf_name
	| TField _ -> "FOther"
	| TLocal v -> "Local:" ^ v.v_name
	| TConst _ -> "Const"
	| _ -> "Other"

let gen_trace ctx args =
	match args with
	| { eexpr = TConst (TString s) } :: _ ->
		let (name, len) = intern_string ctx s in
		emit ctx (Printf.sprintf "call @sa_print_bytes(&%s, %d)" name len)
	| _ ->
		comment ctx "SA-TODO(v0.2): trace of non-literal (needs fmt buffer)"


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
let rec collect_vars acc e =
	match e.eexpr with
	| TVar (v, _) -> v :: acc
	| TBlock el -> List.fold_left collect_vars acc el
	| TIf (c, t, eo) ->
		let acc = collect_vars acc c in
		let acc = collect_vars acc t in
		(match eo with Some x -> collect_vars acc x | None -> acc)
	| TWhile (c, b, _) -> collect_vars (collect_vars acc c) b
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
let rec has_direct_jump depth e =
	match e.eexpr with
	| TBreak | TContinue -> depth = 0
	| TWhile _ -> false
	| TFunction _ -> false
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
	| TTry (e1, catches) ->
		has_direct_jump depth e1
		|| List.exists (fun (_, e2) -> has_direct_jump depth e2) catches
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
	structured switch in SA). Subject evaluated once; arm temps
	released symmetrically like `if` arms. *)
and gen_switch ctx sw =
	let all_pats = List.concat (List.map (fun c -> c.case_patterns) sw.switch_cases) in
	if List.exists (fun p -> switch_pat_const p = None) all_pats then begin
		comment ctx "SA-TODO(v0.6): exotic switch patterns (string/guard/payload)";
		false
	end else begin
		let so = gen_operand ctx sw.switch_subject in
		let ss = match so with Imm s -> s | Reg r -> r in
		let l_end = fresh_label ctx "ENDSWITCH" in
		let l_def = match sw.switch_default with
			| Some _ -> Some (fresh_label ctx "SWDEF")
			| None -> None in
		let tag = ctx.next_label in
		ctx.next_label <- ctx.next_label + 1;
		let arms = List.mapi (fun i c ->
			(Printf.sprintf "L_ARM%d_%d" tag i, c)) sw.switch_cases in
		let rec emit_tests = function
			| [] -> ()
			| (arm_label, c) :: rest ->
				let ft = match rest, l_def with
					| [], None -> l_end
					| [], Some d -> d
					| _ -> fresh_label ctx "SWNEXT" in
				if c.case_patterns = [] then
					emit ctx (Printf.sprintf "jmp %s" arm_label)
				else begin
				let rec one_pat = function
					| [] -> ()
					| [p] ->
						let ps = match switch_pat_const p with
							| Some s -> s | None -> "0" in
						let t = fresh ctx "t" in
						emit ctx (Printf.sprintf "%s = eq %s, %s" t ss ps);
						track ctx t;
						emit ctx (Printf.sprintf "br %s -> %s, %s" t arm_label ft);
						let is_fall = match l_def with
							| Some d -> ft <> l_end && ft <> d
							| None -> ft <> l_end in
						if is_fall then emit_label ctx ft
					| p :: ps ->
						let pcs = match switch_pat_const p with
							| Some s -> s | None -> "0" in
						let t = fresh ctx "t" in
						emit ctx (Printf.sprintf "%s = eq %s, %s" t ss pcs);
						track ctx t;
						let l_or = fresh_label ctx "SWOR" in
						emit ctx (Printf.sprintf "br %s -> %s, %s" t arm_label l_or);
						emit_label ctx l_or;
						one_pat ps
				in
				one_pat c.case_patterns;
				emit_tests rest
				end
		in
		emit_tests arms;
		let all_term = ref true in
		List.iter (fun (arm_label, c) ->
			emit_label ctx arm_label;
			let snap = snapshot ctx in
			let term = gen_stmt ctx c.case_expr in
			release_since ctx snap;
			if not term then emit ctx (Printf.sprintf "jmp %s" l_end);
			all_term := !all_term && term
		) arms;
		(match sw.switch_default, l_def with
		| Some d, Some ld ->
			emit_label ctx ld;
			let snap = snapshot ctx in
			let term = gen_stmt ctx d in
			release_since ctx snap;
			if not term then emit ctx (Printf.sprintf "jmp %s" l_end);
			all_term := !all_term && term
		| _ -> ());
		emit_label ctx l_end;
		!all_term
	end

(* Joins the gen_operand/gen_switch `rec` chain above: switch arms,
	if branches and loop bodies lower statements, statements contain
	expressions and nested control flow. *)
and gen_stmt ctx e =
	match e.eexpr with
	| TBlock el ->
		let term = ref false in
		List.iter (fun s -> term := gen_stmt ctx s) el;
		!term
	| TVar (v, init) ->
		let slot =
			try Hashtbl.find ctx.vars v.v_id
			with Not_found ->
				let s = fresh ctx "var" in
				Hashtbl.replace ctx.vars v.v_id s;
				emit ctx (Printf.sprintf "%s = stack_alloc 8" s);
				track ctx s;
				s
		in
		let ty = mem_ty v.v_type in
		let valu = match init with
			| Some ie -> begin match gen_operand ctx ie with
				| Imm s -> s | Reg r -> r end
			| None -> "0"
		in
		emit ctx (Printf.sprintf "store %s+0, %s as %s" slot valu ty);
		false
	| TReturn ret ->
		let keep = match ret with
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
	| TCall (fn, _) ->
		comment ctx ("SA-TODO(v0.4): general call (" ^ callee_kind fn ^ ")");
		false
	| TBinop (OpAssign, _, _) ->
		ignore (gen_operand ctx e);
		false
	| TIf (cond, then_e, else_opt) ->
		let co = gen_operand ctx cond in
		let cs = match co with Imm s -> s | Reg r -> r in
		let l_then = fresh_label ctx "THEN" in
		let l_end = fresh_label ctx "ENDIF" in
		(match else_opt with
		| Some else_e ->
			let l_else = fresh_label ctx "ELSE" in
			emit ctx (Printf.sprintf "br %s -> %s, %s" cs l_then l_else);
			let snap = snapshot ctx in
			emit_label ctx l_then;
			let term_then = gen_stmt ctx then_e in
			release_since ctx snap;
			if not term_then then emit ctx (Printf.sprintf "jmp %s" l_end);
			emit_label ctx l_else;
			let term_else = gen_stmt ctx else_e in
			release_since ctx snap;
			if not term_else then emit ctx (Printf.sprintf "jmp %s" l_end);
			emit_label ctx l_end;
			term_then && term_else
		| None ->
			emit ctx (Printf.sprintf "br %s -> %s, %s" cs l_then l_end);
			let snap = snapshot ctx in
			emit_label ctx l_then;
			let term_then = gen_stmt ctx then_e in
			release_since ctx snap;
			if not term_then then emit ctx (Printf.sprintf "jmp %s" l_end);
			emit_label ctx l_end;
			false)
	| TWhile (cond, body, flag) ->
		let l_cond = fresh_label ctx "COND" in
		let l_body = fresh_label ctx "BODY" in
		let l_end = fresh_label ctx "ENDWHILE" in
		let entry_snap = snapshot ctx in
		let cond_snap = ref entry_snap in
		ctx.loops <- (l_end, l_cond, entry_snap, cond_snap) :: ctx.loops;
		let emit_cond () =
			emit_label ctx l_cond;
			let co = gen_operand ctx cond in
			let cs = match co with Imm s -> s | Reg r -> r in
			emit ctx (Printf.sprintf "br %s -> %s, %s" cs l_body l_end);
			cond_snap := snapshot ctx
		in
		let emit_body () =
			emit_label ctx l_body;
			(* Single incoming edge: drop cond temps so the bottom
				edge matches the entry edge (live = entry_snap). *)
			release_since ctx entry_snap;
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
	| TParenthesis e1 | TMeta (_, e1) -> gen_stmt ctx e1
	| TConst _ | TLocal _ | TBinop _ | TUnop _ | TArray _ | TArrayDecl _
	| TField _ | TObjectDecl _ ->
		ignore (gen_operand ctx e);
		false
	| _ ->
		comment ctx ("SA-TODO: unsupported statement " ^ expr_kind e);
		false

let print_type buf mt =
	let c =
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
	let ctx = {
		com; buf = body; header;
		next_reg = 0; next_str = 0; next_label = 0;
		vars = Hashtbl.create 16; live = []; loops = [];
	} in
	List.iter (print_type types) com.types;
	let (stmts, has_args) = match com.main.main_expr with
		| Some e -> entry_stmts e
		| None -> ([], false)
	in
	if has_args then comment ctx "SA-TODO(v0.4): entry with args";
	if stmts = [] then comment ctx "no haxe main entry";
	(* Hoist every `stack_alloc` above all branches (PhiStateConflict). *)
	List.iter (fun v ->
		if not (Hashtbl.mem ctx.vars v.v_id) then begin
			let slot = fresh ctx "var" in
			Hashtbl.replace ctx.vars v.v_id slot;
			emit ctx (Printf.sprintf "%s = stack_alloc 8" slot);
			track ctx slot
		end
	) (List.concat (List.map (collect_vars []) stmts));
	List.iter (fun s -> ignore (gen_stmt ctx s)) stmts;
	release_all_except ctx None;
	emit ctx "return 0";
	let ch = open_out_bin com.file in
	output_string ch "// Generated by the Haxe SA target v0.4.\n";
	output_string ch "// Straight-line + if/while + array/object lowering; see SA_TARGET.md.\n";
	output_string ch "@import \"sa_std/io/print.sai\"\n\n";
	output_string ch (Buffer.contents header);
	output_string ch "\n";
	output_string ch (Buffer.contents types);
	output_string ch "\n@main() -> i32:\nL_ENTRY:\n";
	output_string ch (Buffer.contents body);
	close_out ch
