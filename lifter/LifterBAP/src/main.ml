open Bap.Std
open Bap_plugins.Std
open Core_kernel.Std
open Yojson.Basic.Util

exception Bad_inst of mem * int * int
exception Create_mem of Error.t
exception Trailing_data of int

exception Unexpected_Expr
exception Unexpected_Stmt

exception Unhandled_CpuExn
exception Unhandled_Special of string

module Dis = Disasm_expert.Basic


(*********)
(* utils *)
(*********)

let wrap t st args = `Assoc [
    ("Type", `String t) ;
    ("SubType", `String st) ;
    ("Args", `List args)
  ]

let to_binary ?(map=ident) s =
  let seps = [' '; ','; ';'] in
  let separated = List.exists seps ~f:(String.mem s) in
  let bytes = if separated
    then String.split_on_chars ~on:seps s
    else List.init (String.length s / 2) ~f:(fun n ->
        String.slice s (n * 2) (n * 2 + 2)) in
  try bytes |> List.map ~f:map |> String.concat |> Scanf.unescaped
  with Scanf.Scan_failure _ -> raise (Arg.Bad "Bad input")

let create_memory arch s addr =
  let endian = Arch.endian arch in
  Memory.create endian addr @@
  Bigstring.of_string s |> function
    | Ok r -> r
    | Error e -> raise (Create_mem e)

let rec lookup_env v env =
  match env with
  | [] -> None
  | (w, e) :: env_ ->
      if v = w then Some e else lookup_env v env_


(*************)
(* clean bil *)
(*************)

let rec remove_let_expr expr env =
  match expr with
  | Bil.Load (e, e1, endian, s) ->
      let new_e1 = remove_let_expr e1 env in
      Bil.Load (e, new_e1, endian, s)
  | Bil.Store (e, e1, e2, endian, s) ->
      let new_e1 = remove_let_expr e1 env in
      let new_e2 = remove_let_expr e2 env in
      Bil.Store (e, new_e1, new_e2, endian, s)
  | Bil.BinOp (o, e1, e2) ->
      let new_e1 = remove_let_expr e1 env in
      let new_e2 = remove_let_expr e2 env in
      Bil.BinOp (o, new_e1, new_e2)
  | Bil.UnOp (o, e) ->
      let new_e = remove_let_expr e env in
      Bil.UnOp (o, new_e)
  | Bil.Var (v) ->
      let x =
        match lookup_env v env with
        | None -> expr
        | Some (e) -> e
      in
      x
  | Bil.Int (_) -> expr
  | Bil.Cast (c, n, e) ->
      let new_e = remove_let_expr e env in
      Bil.Cast (c, n, new_e)
  | Bil.Let (v, e1, e2) ->
      remove_let_expr e2 ((v, (remove_let_expr e1 env)) :: env)
  | Bil.Unknown (_, _) -> expr
  | Bil.Ite (e1, e2, e3) ->
      let new_e1 = remove_let_expr e1 env in
      let new_e2 = remove_let_expr e2 env in
      let new_e3 = remove_let_expr e3 env in
      Bil.Ite (new_e1, new_e2, new_e3)
  | Bil.Extract (n1, n2, e) ->
      let new_e = remove_let_expr e env in
      Bil.Extract (n1, n2, new_e)
  | Bil.Concat (e1, e2) ->
      let new_e1 = remove_let_expr e1 env in
      let new_e2 = remove_let_expr e2 env in
      Bil.Concat (new_e1, new_e2)

let rec remove_let_stmt stmt =
  match stmt with
  | Bil.Types.Move (v, e) ->
      let new_e = remove_let_expr e [] in
      Bil.Types.Move (v, new_e)
  | Bil.Types.Jmp (e) ->
      let new_e = remove_let_expr e [] in
      Bil.Types.Jmp (new_e)
  | Bil.Types.Special (_) -> stmt
  | Bil.Types.While (e, sl) ->
      let new_e = remove_let_expr e [] in
      let new_sl = List.map ~f:remove_let_stmt sl in
      Bil.Types.While (new_e, new_sl)
  | Bil.Types.If (e, sl1, sl2) ->
      let new_e = remove_let_expr e [] in
      let new_sl1 = List.map ~f:remove_let_stmt sl1 in
      let new_sl2 = List.map ~f:remove_let_stmt sl2 in
      Bil.Types.If (new_e, new_sl1, new_sl2)
  | Bil.Types.CpuExn (_) -> stmt

let remove_let_bil bil =
  List.map ~f:remove_let_stmt bil


(***************)
(* translation *)
(***************)

let json_int i = `Int i

let json_string s = `String s

let json_size size =
  json_int (Size.in_bits size)

let json_reg var =
  let size =
    match Var.typ var with
    | Type.Imm (n) -> n
    | Type.Mem (n, _) -> Size.in_bits n
  in
  [json_string (String.lowercase (Var.name var)) ; json_int size]

let json_endian endian =
  let endian_s =
    match endian with
    | LittleEndian -> "LE"
    | BigEndian -> "BE"
  in
  wrap "EndianT" endian_s []

let json_cast op =
  let op_s =
    match op with
    | Bil.Types.UNSIGNED -> "ZERO"
    | Bil.Types.SIGNED -> "SIGN"
    | Bil.Types.HIGH -> "HIGH"
    | Bil.Types.LOW -> "LOW"
  in
  wrap "CastOpT" op_s []


(* operators *)

let json_unop op =
  let wrap t = "UnOp", (wrap "UnOpT" t []) in

  match op with
    | Bil.Types.NEG -> wrap "NEG"
    | Bil.Types.NOT -> wrap "NOT"

let json_binop op =
  let wrap_bin t = "BinOp", (wrap "BinOpT" t []) in
  let wrap_rel t = "RelOp", (wrap "RelOpT" t []) in

  match op with
    (* binary *)
    | Bil.Types.PLUS -> wrap_bin "ADD"
    | Bil.Types.MINUS -> wrap_bin "SUB"
    | Bil.Types.TIMES -> wrap_bin "UMUL"
    | Bil.Types.DIVIDE -> wrap_bin "UDIV"
    | Bil.Types.SDIVIDE -> wrap_bin "SDIV"
    | Bil.Types.MOD -> wrap_bin "UMOD"
    | Bil.Types.SMOD -> wrap_bin "SMOD"
    | Bil.Types.LSHIFT -> wrap_bin "SHL"
    | Bil.Types.RSHIFT -> wrap_bin "USHR"
    | Bil.Types.ARSHIFT -> wrap_bin "SSHR"
    | Bil.Types.AND -> wrap_bin "AND"
    | Bil.Types.OR -> wrap_bin "OR"
    | Bil.Types.XOR -> wrap_bin "XOR"
    (* relational *)
    | Bil.Types.EQ -> wrap_rel "EQ"
    | Bil.Types.NEQ -> wrap_rel "NEQ"
    | Bil.Types.LT -> wrap_rel "ULT"
    | Bil.Types.LE -> wrap_rel "ULE"
    | Bil.Types.SLT -> wrap_rel "SLT"
    | Bil.Types.SLE -> wrap_rel "SLE"


(* expression *)

let rec json_expr expr =
  let wrap_expr st args = wrap "Expr" st args in

  match expr with
  | Bil.Load (_, e1, endian, s) ->
      wrap_expr "Load" [json_expr e1 ; json_size s]

  | Bil.Store (_, e1, e2, endian, _) ->
      wrap "Stmt" "Store" [json_expr e1 ; json_expr e2]

  | Bil.BinOp (op, e1, e2) ->
      let op_s, op_json = json_binop op in
      wrap_expr op_s [op_json ; json_expr e1 ; json_expr e2]

  | Bil.UnOp (op, e) ->
      let op_s, op_json = json_unop op in
      wrap_expr op_s [op_json ; json_expr e]

  | Bil.Var (v) ->
      wrap "Expr" "Var" (json_reg v)

  | Bil.Int (w) ->
      let value = Word.string_of_value ~hex:false w in
      let value = if value = "-1" then "4294967295" else value in
      let size = Word.bitwidth w in
      wrap "Expr" "Num" [json_string value ; json_int size]

  | Bil.Cast (c, n, e) ->
      wrap_expr "Cast" [json_cast c ; json_int n ; json_expr e]

  | Bil.Let (v, e1, e2) -> raise Unexpected_Expr

  | Bil.Unknown (_, _) ->
      wrap_expr "Undefined" []

  | Bil.Ite (e1, e2, e3) ->
      wrap_expr "Ite" [json_expr e1 ; json_expr e2 ; json_expr e3]

  | Bil.Extract (n1, n2, e) ->
      wrap_expr "Cast" [
        (wrap "CastOpT" "HIGH" []) ;
        json_int (n1 - n2 + 1) ;
        wrap_expr "Cast" [
          (wrap "CastOpT" "LOW" []) ;
          json_int (n1 + 1) ;
          json_expr e]]

  | Bil.Concat (e1, e2) ->
      wrap_expr "BinOp" [
        (wrap "BinOpT" "CONCAT" []) ;
        json_expr e1 ;
        json_expr e2]


(* statement *)

let rec json_stmt (ends, idx, res) stmt =
  let wrap_stmt st args = wrap "Stmt" st args in

  match stmt with
  | Bil.Types.Move (v, e) ->
      let s = match e with
        | Bil.Store (_, _, _, _, _) -> json_expr e
        | _ -> wrap_stmt "Move" ((json_reg v) @ [json_expr e])
      in
      (ends, idx, (s :: res))

  | Bil.Types.Jmp (e) ->
      let s = wrap_stmt "End" [json_expr e] in
      (ends, idx, (s :: res))

  | Bil.Types.Special (s) -> raise (Unhandled_Special s)

  | Bil.Types.While (e, sl) ->
      let e_j = json_expr e in
      let s1 = sprintf "Label%d" idx in
      let s2 = sprintf "Label%d" (idx + 1) in
      let lab1 = wrap_stmt "Label" [json_string s1] in
      let lab2 = wrap_stmt "Label" [json_string s2] in
      let s = wrap_stmt "CJump" [e_j ; json_string s1 ; json_string s2] in

      let _, new_idx, sl_j =
        List.fold_left ~f:json_stmt ~init:(ends, idx + 2, []) sl in
      (* add missing end *)
      let new_sl_j =
        match sl_j with
        | [] -> [ends]
        | (`Assoc [("Type", `String "Stmt") ; ("SubType", `String "End") ; _]) :: _ -> sl_j
        | _ :: _ -> ends :: sl_j
      in

      (ends, new_idx, List.concat [(lab2 :: new_sl_j) ; (lab1 :: s :: res)])

  | Bil.Types.If (e, sl1, sl2) ->
      let e_j = json_expr e in
      let s1 = sprintf "Label%d" idx in
      let s2 = sprintf "Label%d" (idx + 1) in
      let lab1 = wrap_stmt "Label" [json_string s1] in
      let lab2 = wrap_stmt "Label" [json_string s2] in
      let s = wrap_stmt "CJump" [e_j ; json_string s1 ; json_string s2] in

      let _, idx1, sl_j1 =
        List.fold_left ~f:json_stmt ~init:(ends, idx + 2, []) sl1 in
      (* add missing end *)
      let new_sl_j1 =
        match sl_j1 with
        | [] -> [ends]
        | (`Assoc [("Type", `String "Stmt") ; ("SubType", `String "End") ; _]) :: _ -> sl_j1
        | _ :: _ -> ends :: sl_j1
      in

      let _, idx2, sl_j2 =
        List.fold_left ~f:json_stmt ~init:(ends, idx1, []) sl2 in
      (* add missing end *)
      let new_sl_j2 =
        match sl_j2 with
        | [] -> [ends]
        | (`Assoc [("Type", `String "Stmt") ; ("SubType", `String "End") ; _]) :: _ -> sl_j2
        | _ :: _ -> ends :: sl_j2
      in

      (ends, idx2, (List.concat [new_sl_j2 ; (lab2 :: new_sl_j1) ; (lab1 :: s :: res)]))

  | Bil.Types.CpuExn (_) -> raise Unhandled_CpuExn


(* abstract syntax tree *)

let json_ast arch len bil =
  (* TODO: pass arch and match instead of argv.(1) *)
  let addr, size = if Sys.argv.(1) = "x86"
                   then 0x8048000 + len , 32
                   else 0x401000 + len , 64 in
  let start_stmt = wrap "Stmt" "Start"
      [json_int addr ; json_int size ; (json_endian (Arch.endian arch))] in
  let end_stmt = wrap "Stmt" "End"
    [wrap "Expr" "Num" [json_int addr ; json_int size]] in

  (* translate *)
  let _, _, stmts_rev = List.fold_left ~f:json_stmt ~init:(end_stmt, 0, []) bil in

  (* add missing end stmt *)
  let stmts_rev' =
    match stmts_rev with
    | [] -> [end_stmt]
    | (`Assoc [("Type", `String "Stmt") ; ("SubType", `String "End") ; _]) :: _ -> stmts_rev
    | _ :: _ -> end_stmt :: stmts_rev
  in

  (* add start stmt *)
  let stmts = start_stmt :: (List.rev stmts_rev') in

  (* wrap in ast *)
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
          let arch =
            match Arch.of_string Sys.argv.(1) with
              | None -> raise (Arg.Bad "Unknown architecture")
              | Some arch -> arch
          in
          let prepend_slash_x x = "\\x" ^ x in
          let opc =
            match String.prefix Sys.argv.(2) 2 with
              | "" | "\n" -> raise (Arg.Bad "Bad input")
              | x -> to_binary ~map:prepend_slash_x Sys.argv.(2)
          in

          (arch, opc)
        end

  with Arg.Bad s ->
    Printf.eprintf "[error] %s\n" s;
    Printf.eprintf "%s\n" usage;
    exit 1

let _ =
  (* parse arguments *)
  let arch, opc = parse_args () in

  (* set params *)
  (* TODO: match arch and not string *)
  let size = if Sys.argv.(1) = "x86" then ":32" else ":64" in
  let addr = Addr.of_string (if Sys.argv.(1) = "x86"
                             then "0x08048000" ^ size
                             else "0x401000" ^ size) in
  let mem = create_memory arch opc addr in
  let backend = "llvm" in

  (* lift *)
  ignore (Plugins.load ());
  Dis.with_disasm ~backend (Arch.to_string arch) ~f:(fun dis ->
    (* disassemble *)
    let bytes = Dis.run dis mem ~return:ident ~init:0 ~stop_on:[`Valid]
        (* fail *)
        ~invalid:(fun state mem start ->
          let json = wrap "AST" "Uninterpretable" [] in
          printf "%s\n" (Yojson.Basic.pretty_to_string json);
          0
          )

        (* success *)
        ~hit:(fun state mem insn bytes ->

          let module Target = (val target_of_arch arch) in

          (* translate json *)
          let bil =
            match Target.lift mem insn with
              | Ok bil -> bil
              | Error e -> [Bil.special @@ sprintf "Lifter: %s" @@ Error.to_string_hum e]
          in
          let bil' = remove_let_bil bil in
          let json =
            try
              json_ast arch (Memory.length mem) bil'
            with
            | Unhandled_Special (_) -> wrap "AST" "Uninterpretable" []
            | Unhandled_CpuExn -> wrap "AST" "Uninterpretable" []
          in

          (* pretty print *)
          printf "%s\n" (Yojson.Basic.pretty_to_string json);

          Dis.stop state bytes
        ) in

    match String.length opc - bytes with
    | 0 -> Or_error.return ()
  (*  | n -> raise (Trailing_data n) *)
    | _ -> Or_error.return ())
