module MeanDiff.JSONParse

open FSharp.Data
open System

open UIR

let getTypeField (json : JsonValue) =
  match (json.TryGetProperty "Type") with
  | Some x -> x.AsString()
  | None -> raise FieldNotFound

let getSubTypeField (json : JsonValue) =
  match (json.TryGetProperty "SubType") with
  | Some x -> x.AsString()
  | None -> raise FieldNotFound

let getArgsField (json : JsonValue) =
  match (json.TryGetProperty "Args") with
  | Some x -> x.AsArray()
  | None -> raise FieldNotFound

let chkType s = function
  | x when x = s -> ()
  | _ -> raise InvalidTypeField

let chkLength s = function
  | x when s = Array.length x -> ()
  | _ -> raise InvalidArgsLength

let toValue (json : JsonValue) = json.AsString() |> Convert.ToUInt64

let toSize (json : JsonValue) = json.AsString() |> Convert.ToUInt16

let toSymbol (json : JsonValue) = json.AsString()

let toEndianT json =
  let ty = getTypeField json
  let sty = getSubTypeField json
  chkType "EndianT" ty
  match sty with
  | "BE" -> BE
  | "LE" -> LE
  | _ -> raise InvalidSubTypeField

let toUnOpT json =
  let ty = getTypeField json
  let sty = getSubTypeField json
  chkType "UnOpT" ty
  match sty with
  | "NEG" -> NEG
  | "NOT" -> NOT
  | _ -> raise InvalidSubTypeField

let toBinOpT json =
  let ty = getTypeField json
  let sty = getSubTypeField json
  chkType "BinOpT" ty
  match sty with
  | "ADD" -> ADD
  | "SUB" -> SUB
  | "UMUL" -> UMUL
  | "SMUL" -> SMUL
  | "UDIV" -> UDIV
  | "SDIV" -> SDIV
  | "UMOD" -> UMOD
  | "SMOD" -> SMOD
  | "SHL" -> SHL
  | "USHR" -> USHR
  | "SSHR" -> SSHR
  | "AND" -> AND
  | "OR" -> OR
  | "XOR" -> XOR
  | "CONCAT" -> CONCAT
  | _ -> raise InvalidSubTypeField

let toRelOpT json =
  let ty = getTypeField json
  let sty = getSubTypeField json
  chkType "RelOpT" ty
  match sty with
  | "EQ" -> EQ
  | "NEQ" -> NEQ
  | "ULT" -> ULT
  | "SLT" -> SLT
  | "ULE" -> ULE
  | "SLE" -> SLE
  | _ -> raise InvalidSubTypeField

let toCastOpT json =
  let ty = getTypeField json
  let sty = getSubTypeField json
  chkType "CastOpT" ty
  match sty with
  | "LOW" -> LOW
  | "HIGH" -> HIGH
  | "ZERO" -> ZERO
  | "SIGN" -> SIGN
  | _ -> raise InvalidSubTypeField

let rec toExpr json =
  let ty = getTypeField json
  let sty = getSubTypeField json
  chkType "Expr" ty
  match sty with
  | "Num" ->
      let args = getArgsField json
      chkLength 2 args
      (toValue args.[0], toSize args.[1]) |> Num
  | "Var" ->
      let args = getArgsField json
      chkLength 2 args
      (toSymbol args.[0], toSize args.[1]) |> Var
  | "Load" ->
      let args = getArgsField json
      chkLength 2 args
      (toExpr args.[0], toSize args.[1]) |> Load
  | "UnOp" ->
      let args = getArgsField json
      chkLength 2 args
      (toUnOpT args.[0], toExpr args.[1]) |> UnOp
  | "BinOp" ->
      let args = getArgsField json
      chkLength 3 args
      (toBinOpT args.[0], toExpr args.[1], toExpr args.[2]) |> BinOp
  | "RelOp" ->
      let args = getArgsField json
      chkLength 3 args
      (toRelOpT args.[0], toExpr args.[1], toExpr args.[2]) |> RelOp
  | "Cast" ->
      let args = getArgsField json
      chkLength 3 args
      (toCastOpT args.[0], toSize args.[1], toExpr args.[2]) |> Cast
  | "Ite" ->
      let args = getArgsField json
      chkLength 3 args
      (toExpr args.[0], toExpr args.[1], toExpr args.[2]) |> Ite
  | "Undefined" -> Undefined
  | _ -> raise InvalidSubTypeField

let toStmt json =
  let ty = getTypeField json
  let sty = getSubTypeField json
  chkType "Stmt" ty
  match sty with
  | "Start" ->
      let args = getArgsField json
      chkLength 3 args
      (toValue args.[0], toSize args.[1], toEndianT args.[2]) |> Start
  | "Move" ->
      let args = getArgsField json
      chkLength 3 args
      ((toSymbol args.[0], toSize args.[1]), toExpr args.[2]) |> Move
  | "Store" ->
      let args = getArgsField json
      chkLength 2 args
      (toExpr args.[0], toExpr args.[1]) |> Store
  | "Label" ->
      let args = getArgsField json
      chkLength 1 args
      toSymbol args.[0] |> Label
  | "CJump" ->
      let args = getArgsField json
      chkLength 3 args
      (toExpr args.[0], toSymbol args.[1], toSymbol args.[2]) |> CJump
  | "End" ->
      let args = getArgsField json
      chkLength 1 args
      toExpr args.[0] |> End
  | "Unrecognized" -> Unrecognized
  | _ -> raise InvalidSubTypeField

let toAST s =
  let json = JsonValue.Parse s
  let ty = getTypeField json
  let sty = getSubTypeField json
  chkType "AST" ty
  match sty with
  | "Stmts" ->
      let args = getArgsField json
      args |> List.ofArray |> List.map toStmt |> Stmts
  | "Uninterpretable" -> Uninterpretable
  | "Incapable" -> Incapable
  | _ -> raise InvalidSubTypeField
