open Format
open Dba
open Dba_types
open String


(**************)
(* exceptions *)
(**************)

exception UnhandledOp of string
exception UnhandledArch of string
exception UnhandledInsn of string


(*********)
(* utils *)
(*********)

let wrap t st args = `Assoc [
    ("Type", `String t) ;
    ("SubType", `String st) ;
    ("Args", `List args)
  ]

let unrestrict lhs expr name size lo hi =
  let lhs = Dba.LhsVar (name, size, None) in
  let v = Dba.ExprVar (name, size, None) in
  let expr =
    if hi + 1 < size - 1
    then
      Dba.ExprBinary (Dba.Concat, Dba.ExprRestrict (v, hi + 1, size - 1), expr)
    else
      expr
    in
  let expr =
    if 0 < lo - 1
    then
      Dba.ExprBinary (Dba.Concat, expr, Dba.ExprRestrict (v, 0, lo - 1))
    else
      expr
    in
  lhs, expr


(***************)
(* translators *)
(***************)

(* types *)

let json_string s = `String (String.lowercase s)

let json_int i = `Int i

let json_size = json_int

(* NOTE: all instructions is max 8 bytes as numbers *)
let json_addr base addr =
  let i = (base + (Bigint.int_of_big_int (Bitvector.value_of addr.base))) in
  json_int i, json_size (Bitvector.size_of addr.base)


(* endianness *)

let json_endian endian =
  match endian with
  | Dba.LittleEndian -> wrap "EndianT" "LE" []
  | Dba.BigEndian -> wrap "EndianT" "BE" []


(* unary operators *)

let json_unop op =
  let wrap t = "UnOp", (wrap "UnOpT" t []) in

  match op with
  | Dba.UMinus -> wrap "NEG"
  | Dba.Not -> wrap "NOT"


(* binary operators *)

let json_binop op =
  let wrap_bin t = "BinOp", (wrap "BinOpT" t []) in
  let wrap_rel t = "RelOp", (wrap "RelOpT" t []) in

  match op with
  (* binary *)
  | Dba.Plus -> wrap_bin "ADD"
  | Dba.Minus -> wrap_bin "SUB"
  | Dba.MultU -> wrap_bin "UMUL"
  | Dba.MultS -> wrap_bin "SMUL"
  | Dba.DivU -> wrap_bin "UDIV"
  | Dba.DivS -> wrap_bin "SDIV"
  | Dba.ModU -> wrap_bin "UMOD"
  | Dba.ModS -> wrap_bin "SMOD"
  | Dba.Or -> wrap_bin "OR"
  | Dba.And -> wrap_bin "AND"
  | Dba.Xor -> wrap_bin "XOR"
  | Dba.Concat -> wrap_bin "CONCAT"
  | Dba.LShift -> wrap_bin "SHL"
  | Dba.RShiftU -> wrap_bin "USHR"
  | Dba.RShiftS -> wrap_bin "SSHR"
  | Dba.LeftRotate -> raise (UnhandledOp "LeftRotate")
  | Dba.RightRotate -> raise (UnhandledOp "RightRotate")
  (* relational *)
  | Dba.Eq -> wrap_rel "EQ"
  | Dba.Diff -> wrap_rel "NEQ"
  | Dba.LeqU -> wrap_rel "ULE"
  | Dba.LtU -> wrap_rel "ULT"
  | Dba.GeqU -> raise (UnhandledOp "GeqU")
  | Dba.GtU -> raise (UnhandledOp "GtU")
  | Dba.LeqS -> wrap_rel "SLE"
  | Dba.LtS -> wrap_rel "SLT"
  | Dba.GeqS -> raise (UnhandledOp "GeqS")
  | Dba.GtS -> raise (UnhandledOp "GtS")


(* expression *)

let rec json_expr expr =
  let wrap_expr st args = wrap "Expr" st args in

  match expr with
  | Dba.ExprVar (name, size, tag) -> begin
      match tag with
      (* | Some _ -> raise (UnhandledOp "vartag") *) (* TODO: unhandled vartag *)
      | _ -> wrap_expr "Var" [json_string name ; json_size size]
    end

  | Dba.ExprLoad (size, endian, e) ->
      (* NOTE: size is in bytes *)
      wrap_expr "Load" [json_expr e ; json_size (size * 8)]

  | Dba.ExprCst (_, vector) ->
      let value = Bigint.int_of_big_int (Bitvector.value_of vector) in
      let size = Bitvector.size_of vector in
      wrap_expr "Num" [json_int value ; json_size size]

  | Dba.ExprUnary (op, e) ->
      let op_s, op_json = json_unop op in
      wrap_expr op_s [op_json ; json_expr e]

  | Dba.ExprBinary (op, e1, e2) -> begin
      match op with
      | Dba.GtU -> json_expr (Expr.unary Dba.Not (Expr.binary Dba.LeqU e1 e2))
      | Dba.GeqU -> json_expr (Expr.unary Dba.Not (Expr.binary Dba.LtU e1 e2))
      | Dba.GtS -> json_expr (Expr.unary Dba.Not (Expr.binary Dba.LeqS e1 e2))
      | Dba.GeqS -> json_expr (Expr.unary Dba.Not (Expr.binary Dba.LtS e1 e2))
      | _ ->
          let op_s, op_json = json_binop op in
          wrap_expr op_s [op_json ; json_expr e1 ; json_expr e2]
    end

  | Dba.ExprRestrict (expr, lo, hi) ->
      let c' = wrap_expr "Cast" [
        (wrap "CastOpT" "LOW" []) ;
        json_int (hi + 1) ;
        json_expr expr
      ] in
      wrap_expr "Cast" [
        (wrap "CastOpT" "HIGH" []) ;
        json_int (hi - lo + 1) ;
        c'
      ]

  | Dba.ExprExtU (expr, size) ->
      wrap_expr "Cast" [(wrap "CastOpT" "ZERO" []) ; json_size size ; json_expr expr]

  | Dba.ExprExtS (expr, size) ->
      wrap_expr "Cast" [(wrap "CastOpT" "SIGN" []) ; json_size size ; json_expr expr]

  | Dba.ExprIte (c, e1, e2) ->
      wrap_expr "Ite" [json_cond c ; json_expr e1 ; json_expr e2]

  | Dba.ExprAlternative (_, _) -> raise (UnhandledOp "ExprAlternative")
    (* let op_s, op_json = json_binop Dba.Concat in *)
    (* let rec fold exprs = *)
    (*   match exprs with *)
    (*   | [] -> raise (UnhandledOp "empty ExprAlternative") *)
    (*   | [e] -> json_expr e *)
    (*   | e :: es -> wrap_expr op_s ([op_json ; json_expr e] @ [fold es]) *)
    (* in *)
    (* fold exprs *)


(* condition *)

and json_cond cond =
  match cond with
  | Dba.CondReif (e) -> json_expr e
  | Dba.CondNot (c) ->
      let op_s, op_json = json_unop Dba.Not in
      wrap "Expr" op_s [op_json ; json_cond c]
  | Dba.CondAnd (c1, c2) ->
      let op_s, op_json = json_binop Dba.And in
      wrap "Expr" op_s [op_json ; json_cond c1 ; json_cond c2]
  | Dba.CondOr (c1, c2) ->
      let op_s, op_json = json_binop Dba.Or in
      wrap "Expr" op_s [op_json ; json_cond c1 ; json_cond c2]
  | Dba.True ->
      wrap "Expr" "Num" [json_int 1 ; json_size 1]
  | Dba.False ->
      wrap "Expr" "Num" [json_int 0 ; json_size 1]


(* left-hand-side *)

let json_lhs lhs =
  match lhs with
  | Dba.LhsVar (name, size, _) ->
      [json_string name ; json_size size]
  | Dba.LhsVarRestrict (name, size, _, _) ->
      [json_string name ; json_size size]
  | Dba.LhsStore (_, _, _) -> raise (UnhandledOp "LhsStore")


(* target *)

let json_target target base_addr =
  match target with
  | Dba.JInner (id) ->
      wrap "Expr" "Num" [json_int id ; json_int 8]
  | Dba.JOuter (addr) ->
    let i_json, s_json = json_addr base_addr addr in
      wrap "Expr" "Num" [i_json ; s_json]


(* statement *)

let json_stmt (addr, idx, res, num, label) s =
  let wrap_stmt st args = wrap "Stmt" st args in

  let res, label =
    match label with
    | Some (n, j) -> if num = n then j :: res, None else res, label
    | None -> res, label
  in

  match s with
  | Dba.IkAssign (lhs, expr, _) -> begin
      let j =
        match lhs with
        | Dba.LhsVar (_, _, _) ->
            wrap_stmt "Move" (json_lhs lhs @ [json_expr expr])
        | Dba.LhsVarRestrict (name, size, l, h) ->
            let lhs, expr = unrestrict lhs expr name size l h in
            wrap_stmt "Move" (json_lhs lhs @ [json_expr expr])
        | Dba.LhsStore (_, endian, e2) ->
            wrap_stmt "Store" [json_expr e2 ; json_expr expr]
      in
      (addr, idx, j :: res, num + 1, label)
    end

  | Dba.IkSJump (target, _) ->
      let e = json_target target addr in
      let j = wrap_stmt "End" [e] in
      (addr, idx, j :: res, num + 1, label)

  | Dba.IkDJump (expr, _) ->
      let j = wrap_stmt "End" [json_expr expr] in
      (addr, idx, j :: res, num + 1, label)

  | Dba.IkIf (cond, target, _) ->
      let c = json_cond cond in
      let s1 = json_string (sprintf "Label%d" idx) in
      let s2 = json_string (sprintf "Label%d" (idx + 1)) in
      let lab1 = wrap_stmt "Label" [s1] in
      let lab2 = wrap_stmt "Label" [s2] in
      let swt = wrap_stmt "CJump" [c ; s1 ; s2] in
      let target =
        match target with
        | JInner (id) -> id
        | _ -> raise (UnhandledInsn "IkIf")
      in
      (addr, idx + 2, lab2 :: swt :: res, num + 1, Some (target, lab1))

  | Dba.IkStop (_) ->
      raise (UnhandledInsn "IkStop")

  | Dba.IkAssert (_, _) -> raise (UnhandledOp "IkAssert")
  | Dba.IkAssume (_, _) -> raise (UnhandledOp "IkAssume")
  | Dba.IkNondetAssume (_, _, _) -> raise (UnhandledOp "IkNondetAssume")
  | Dba.IkNondet (_, _, _) -> raise (UnhandledOp "IkNondet")

  | Dba.IkUndef (lhs, _) ->
      let j = wrap_stmt "Move" (json_lhs lhs @ [wrap "Expr" "Undefined" []]) in
      (addr, idx, j :: res, num + 1, label)

  | Dba.IkMalloc (_, _, _) -> raise (UnhandledOp "IkMalloc")
  | Dba.IkFree (_, _) -> raise (UnhandledOp "IkFree")
  | Dba.IkPrint (_, _) -> raise (UnhandledOp "IkPrint")


(* abstract syntax tree *)

let json_ast addr len dba =
  let start_stmt = wrap "Stmt" "Start"
      [json_int addr ; json_size 32 ; (json_endian Dba.LittleEndian)] in
  let end_stmt = wrap "Stmt" "End" [
      wrap "Expr" "Num" [json_int (addr + len) ; json_size 32]
    ] in

  (* translate each stmt to json *)
  let _, _, rev_stmts, _, _ = Block.fold_left json_stmt (addr, 0, [], 0, None) dba in

  (* add last end stmt if not already there *)
  let rev_stmts' = if (List.nth rev_stmts 0) = end_stmt
    then rev_stmts
    else end_stmt :: rev_stmts in

  (* add start stmt *)
  let stmts = start_stmt :: (List.rev rev_stmts') in

  (* wrap stmts in ast *)
  wrap "AST" "Stmts" stmts


(********)
(* main *)
(********)

let usage = "usage: " ^ Sys.argv.(0) ^ " <arch> <opcode>"

let parse_args () =
  let len = Array.length Sys.argv in

  try
    match len with
    | x when x <> 3 -> raise (Arg.Bad "Wrong number of arguments given")
    | _ ->
      begin
        let arch = match (String.lowercase Sys.argv.(1)) with
          | "x86" -> "x86"
          | "x64" | "x86_64" -> raise (UnhandledArch Sys.argv.(1))
          | _ -> raise (Arg.Bad "Unknown architecture")
        in
        let opc = Sys.argv.(2) in

        (arch, opc)
      end

  with Arg.Bad s ->
    Logger.fatal "%s" s;
    Printf.eprintf "%s" usage;
    exit(1)


let main =
  Logger.set_log_level "fatal";
  (* uncomment to enable debug *)
  (* Logger.set_log_level "debug"; *)

  (* translate into json *)
  let json =
    try
      (* get command line args *)
      let arch, opc = parse_args () in

      (* lift instruction *)
      let mnemonic, dba = Decode_utils.decode_hex_opcode opc in

      (* print dba *)
      Logger.debug "\n\n%s\n=======================================" mnemonic;
      Block.iter (fun i -> Logger.debug "%a" Dba_printer.Ascii.pp_instruction i) dba;
      Logger.debug "\n\n";

      (* variables *)
      let addr = 0x8048000 in
      let len = (Block.length dba) * 32 in

      (* translate *)
      json_ast addr len dba

    with
    | UnhandledArch s ->
      Logger.fatal "Unhandled architecture %s" s;
      wrap "AST" "Incapable" []
    | UnhandledOp op ->
      Logger.warning "Unhandled %s" op;
      wrap "AST" "Incapable" []
    | UnhandledInsn _ ->
      wrap "AST" "Incapable" []
    | _ ->
      wrap "AST" "Uninterpretable" []
  in

  (* print the list, line by line *)
  Logger.debug "\n=======================================";
  print_endline (Yojson.Basic.pretty_to_string json)

let () = main
