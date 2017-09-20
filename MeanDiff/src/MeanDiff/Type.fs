module MeanDiff.Type

open UIR

module SymbTable =
  type Table = Map<Symbol, Size>

  let createTable = Map.empty

  let checkElem symb (table : Table) = table.TryFind symb

  let setElem symb size (table : Table) = table.Add(symb, size)

  let getElem symb (table : Table) = table.Item symb

let rec getSizeFromExpr = function
  | Num (_, size)
  | Var (_, size) ->
      size
  | Load (addr, size) ->
      let addrSize = addr |> getSizeFromExpr
      if addrSize <> 0us
      then
        size
      else
        0us
  | UnOp (_, expr) -> expr |> getSizeFromExpr
  | BinOp (op, expr1, expr2) ->
      let size1 = expr1 |> getSizeFromExpr
      let size2 = expr2 |> getSizeFromExpr
      if size1 <> 0us && size2 <> 0us
      then
        if op = CONCAT
        then
          size1 + size2
        else
          size1
      else
        0us
  | RelOp (_, expr1, expr2) ->
      let size1 = expr1 |> getSizeFromExpr
      let size2 = expr2 |> getSizeFromExpr
      if size1 <> 0us && size2 <> 0us
      then
        1us
      else
        0us
  | Cast (_, size, expr) ->
      let exprSize = expr |> getSizeFromExpr
      if exprSize <> 0us
      then
        size
      else
        0us
  | Ite (cond, thenExpr, elseExpr) ->
      let condSize = cond |> getSizeFromExpr
      let thenSize = thenExpr |> getSizeFromExpr
      let elseSize = elseExpr |> getSizeFromExpr
      if condSize = 1us && thenSize <> 0us && elseSize <> 0us
      then
        thenSize
      else
        0us
  | Undefined -> 0us

let getTypeReg symbTable (name, size) =
  match SymbTable.checkElem name symbTable with
  | Some (oldSize) ->
      if oldSize = size
      then
        size, symbTable
      else
        printfn "%A" (name, size)
        raise VariableSizeMismatch
  | None -> size, (SymbTable.setElem name size symbTable)

let rec getTypeExpr symbTable = function
  | Num (_, size) -> size, symbTable
  | Var (reg) -> reg |> getTypeReg symbTable
  | Load (addr, size) ->
      let size1, symbTable = addr |> getTypeExpr symbTable
      if size1 = 0us
      then
        0us, symbTable
      else
        size, symbTable
  | UnOp (_, expr) -> expr |> getTypeExpr symbTable
  | BinOp (CONCAT, expr1, expr2) ->
      let size1, symbTable = expr1 |> getTypeExpr symbTable
      let size2, symbTable = expr2 |> getTypeExpr symbTable
      if size1 = 0us || size2 = 0us
      then
        0us, symbTable
      else
        size1 + size2, symbTable
  | BinOp (SHL, expr1, expr2)
  | BinOp (USHR, expr1, expr2)
  | BinOp (SSHR, expr1, expr2) ->
      let size1, symbTable = expr1 |> getTypeExpr symbTable
      let size2, symbTable = expr2 |> getTypeExpr symbTable
      if size1 = 0us || size2 = 0us
      then
        0us, symbTable
      else
        if size1 >= size2
        then
          size1, symbTable
        else
          raise OperandSizeMismatch
  | BinOp (_, expr1, expr2) ->
      let size1, symbTable = expr1 |> getTypeExpr symbTable
      let size2, symbTable = expr2 |> getTypeExpr symbTable
      if size1 = 0us || size2 = 0us
      then
        0us, symbTable
      else
        if size1 = size2
        then
          size1, symbTable
        else
          raise OperandSizeMismatch
  | RelOp (_, expr1, expr2) ->
      let size1, symbTable = expr1 |> getTypeExpr symbTable
      let size2, symbTable = expr2 |> getTypeExpr symbTable
      if size1 = 0us || size2 = 0us
      then
        0us, symbTable
      else
        if size1 = size2
        then
          1us, symbTable
        else
          raise OperandSizeMismatch
  | Cast (LOW, size, expr)
  | Cast (HIGH, size, expr) ->
      let size1, symbTable = expr |> getTypeExpr symbTable
      if size1 = 0us
      then
        0us, symbTable
      else
        if size <= size1
        then
          size, symbTable
        else
          raise InvalidCastSize
  | Cast (ZERO, size, expr)
  | Cast (SIGN, size, expr) ->
      let size1, symbTable = expr |> getTypeExpr symbTable
      if size1 = 0us
      then
        0us, symbTable
      else
        if size1 <= size
        then
          size, symbTable
        else
          raise InvalidCastSize
  | Ite (cond, thenExpr, elseExpr) ->
      let size1, symbTable = cond |> getTypeExpr symbTable
      let size2, symbTable = thenExpr |> getTypeExpr symbTable
      let size3, symbTable = elseExpr |> getTypeExpr symbTable
      if size1 = 0us || size2 = 0us || size3 = 0us
      then
        0us, symbTable
      else
        if size1 = 1us
        then
          if size2 = size3
          then
            size2, symbTable
          else
            raise OperandSizeMismatch
        else
          raise OperandSizeMismatch
  | Undefined -> 0us, symbTable

let typeCheckStmt symbTable = function
  | Start (_, _, _) -> symbTable
  | Move (reg, expr) ->
      let regSize, symbTable = reg |> getTypeReg symbTable
      let exprSize, symbTable = expr |> getTypeExpr symbTable
      if exprSize = 0us
      then
        symbTable
      else
        if regSize = exprSize
        then
          symbTable
        else
          raise OperandSizeMismatch
  | Store (addr, expr) ->
      let _, symbTable = addr |> getTypeExpr symbTable
      let _, symbTable = expr |> getTypeExpr symbTable
      symbTable
  | Label (_) -> symbTable
  | CJump (cond, _, _) ->
      let size, symbTable = cond |> getTypeExpr symbTable
      if size = 0us
      then
        symbTable
      else
        if size = 1us
        then
          symbTable
        else
          raise OperandSizeMismatch
  | End (addr) ->
      let _, symbTable = addr |> getTypeExpr symbTable
      symbTable
  | Unrecognized -> symbTable

let typeCheck = function
  | Stmts (sl) ->
      sl |> List.fold typeCheckStmt (SymbTable.createTable) |> ignore
  | Uninterpretable
  | Incapable -> ()
