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
	vars : (int, string) Hashtbl.t;
	mutable live : string list;
}

let fresh ctx prefix =
	let id = ctx.next_reg in
	ctx.next_reg <- id + 1;
	Printf.sprintf "%s%d" prefix id

let emit ctx s =
	Buffer.add_string ctx.buf "    ";
	Buffer.add_string ctx.buf s;
	Buffer.add_char ctx.buf '\n'

let comment ctx s =
	Buffer.add_string ctx.buf "    // ";
	Buffer.add_string ctx.buf s;
	Buffer.add_char ctx.buf '\n'

let track ctx r =
	ctx.live <- r :: ctx.live

(** True when the (followed) type is Haxe Float. Everything else is
	treated as i32 for v0.2 (ints, bools as 0/1, chars). *)
let is_float_t t =
	match follow t with
	| TAbstract ({ a_path = ([], "Float") }, _) -> true
	| TInst ({ cl_path = ([], "Float") }, _) -> true
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

(** Materialize a string literal as `@const STR_n = utf8:"..."` and
	return its name. Byte length = OCaml String.length (UTF-8 bytes). *)
let intern_string ctx s =
	let id = ctx.next_str in
	ctx.next_str <- id + 1;
	let name = Printf.sprintf "STR_%d" id in
	Buffer.add_string ctx.header
		(Printf.sprintf "@const %s = utf8:\"%s\"\n" name (sa_escape s));
	(name, String.length s)

let rec gen_operand ctx e =
	match e.eexpr with
	| TConst (TInt i) -> Imm (Int32.to_string i)
	| TConst (TFloat f) -> Imm f
	| TConst (TBool b) -> Imm (if b then "1" else "0")
	| TConst TNull -> Imm "0"
	| TLocal v -> begin
		try
			let slot = Hashtbl.find ctx.vars v.v_id in
			let r = fresh ctx "t" in
			let ty = if is_float_t v.v_type then "f64" else "i32" in
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
			let ty = if is_float_t v.v_type then "f64" else "i32" in
			let os = match op with Imm s -> s | Reg r -> r in
			emit ctx (Printf.sprintf "store %s+0, %s as %s" slot os ty);
			op
		with Not_found ->
			comment ctx ("SA-TODO(v0.2): assign to unhomed " ^ v.v_name);
			Imm "0"
	end
	| TBinop (op, e1, e2) -> gen_binop ctx op e1 e2 e.etype
	| TParenthesis e1 | TMeta (_, e1) -> gen_operand ctx e1
	| TCast (e1, _) -> gen_operand ctx e1
	| _ ->
		comment ctx "SA-TODO(v0.2/v0.3): unsupported expression";
		Imm "0"

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
		comment ctx "SA-TODO(v0.2): unsupported operator for operand types";
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

let rec gen_stmt ctx e =
	match e.eexpr with
	| TBlock el -> List.iter (gen_stmt ctx) el
	| TVar (v, init) ->
		let slot = fresh ctx "var" in
		Hashtbl.replace ctx.vars v.v_id slot;
		emit ctx (Printf.sprintf "%s = stack_alloc 8" slot);
		track ctx slot;
		let ty = if is_float_t v.v_type then "f64" else "i32" in
		let valu = match init with
			| Some ie -> begin match gen_operand ctx ie with
				| Imm s -> s | Reg r -> r end
			| None -> "0"
		in
		emit ctx (Printf.sprintf "store %s+0, %s as %s" slot valu ty)
	| TReturn ret ->
		let keep = match ret with
			| Some re -> begin match gen_operand ctx re with
				| Imm s -> release_all_except ctx None; Imm s
				| Reg r -> release_all_except ctx (Some r); Reg r end
			| None -> release_all_except ctx None; Imm "0"
		in
		let rs = match keep with Imm s -> s | Reg r -> r in
		emit ctx (Printf.sprintf "return %s" rs)
	| TCall (fn, args) when callee_is_trace fn ->
		gen_trace ctx args
	| TCall (fn, _) ->
		comment ctx ("SA-TODO(v0.4): general call (" ^ callee_kind fn ^ ")")
	| TBinop (OpAssign, _, _) ->
		ignore (gen_operand ctx e)
	| TIf _ | TWhile _ ->
		comment ctx "SA-TODO(v0.3): control flow (br+jmp)"
	| TSwitch _ ->
		comment ctx "SA-TODO(v0.5): switch (eq chain)"
	| TParenthesis e1 | TMeta (_, e1) -> gen_stmt ctx e1
	| TConst _ | TLocal _ | TBinop _ ->
		ignore (gen_operand ctx e)
	| _ ->
		comment ctx "SA-TODO: unsupported statement"

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
		next_reg = 0; next_str = 0;
		vars = Hashtbl.create 16; live = [];
	} in
	List.iter (print_type types) com.types;
	let (stmts, has_args) = match com.main.main_expr with
		| Some e -> entry_stmts e
		| None -> ([], false)
	in
	if has_args then comment ctx "SA-TODO(v0.4): entry with args";
	if stmts = [] then comment ctx "no haxe main entry";
	List.iter (gen_stmt ctx) stmts;
	release_all_except ctx None;
	emit ctx "return 0";
	let ch = open_out_bin com.file in
	output_string ch "// Generated by the Haxe SA target v0.2.\n";
	output_string ch "// Straight-line lowering only; see SA_TARGET.md.\n";
	output_string ch "@import \"sa_std/io/print.sai\"\n\n";
	output_string ch (Buffer.contents header);
	output_string ch "\n";
	output_string ch (Buffer.contents types);
	output_string ch "\n@main() -> i32:\nL_ENTRY:\n";
	output_string ch (Buffer.contents body);
	close_out ch
