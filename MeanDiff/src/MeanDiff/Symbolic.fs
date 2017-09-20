module MeanDiff.Symbolic

open Graph
open Tree
open Type
open UIR

type Context = Map<Reg, Expr>

type Summary = Map<Reg, Tree<Expr, Expr>>

type SummaryInfo = Summary * Map<Expr, int> * Summary

let getIdxFromMemMap expr memMap =
  let memMap =
    if memMap |> Map.containsKey expr
    then
      memMap
    else
      let idx = memMap.Count
      memMap |> Map.add expr idx
  memMap, (memMap.Item expr)

let initRegisterContext context reg =
  context |> Map.add reg (Var reg)

let initContext inVars context =
  inVars |> Set.fold initRegisterContext context

let rec executeExpr memMap (context : Context) = function
  | Var (reg) ->
      memMap, context, (context.Item reg)
  | Load (addr, size) ->
      let memMap, context, addr = addr |> executeExpr memMap context
      let memMap, idx = memMap |> getIdxFromMemMap addr
      let reg = (sprintf "mem%d" idx), size
      let context =
        if context |> Map.containsKey reg
        then
          context
        else
          context |> Map.add reg (Var reg)
      memMap, context, (context.Item reg)
  | UnOp (op, expr) ->
      let memMap, context, expr = expr |> executeExpr memMap context
      memMap, context, UnOp (op, expr)
  | BinOp (op, expr1, expr2) ->
      let memMap, context, expr1 = expr1 |> executeExpr memMap context
      let memMap, context, expr2 = expr2 |> executeExpr memMap context
      memMap, context, BinOp (op, expr1, expr2)
  | RelOp (op, expr1, expr2) ->
      let memMap, context, expr1 = expr1 |> executeExpr memMap context
      let memMap, context, expr2 = expr2 |> executeExpr memMap context
      memMap, context, RelOp (op, expr1, expr2)
  | Cast (op, size, expr) ->
      let memMap, context, expr = expr |> executeExpr memMap context
      memMap, context, Cast (op, size, expr)
  | Ite (cond, thenExpr, elseExpr) ->
      let memMap, context, cond = cond |> executeExpr memMap context
      let memMap, context, thenExpr = thenExpr |> executeExpr memMap context
      let memMap, context, elseExpr = elseExpr |> executeExpr memMap context
      memMap, context, Ite (cond, thenExpr, elseExpr)
  | expr -> memMap, context, expr

let executeStmt memMap context = function
  | Move (reg, expr) ->
      let memMap, context, expr = expr |> executeExpr memMap context
      let context = context |> Map.add reg expr
      memMap, context, None, None
  | Store (addr, expr) ->
      let memMap, context, expr = expr |> executeExpr memMap context
      let size = expr |> getSizeFromExpr
      let memMap, context, addr = addr |> executeExpr memMap context
      let memMap, idx = memMap |> getIdxFromMemMap addr
      let reg = (sprintf "mem%d" idx), size
      let context = context |> Map.add reg expr
      memMap, context, None, None
  | CJump (cond, _, _) ->
      let memMap, context, cond = cond |> executeExpr memMap context
      memMap, context, Some cond, None
  | End (addr) ->
      let memMap, context, addr = addr |> executeExpr memMap context
      memMap, context, None, Some addr
  | _ -> memMap, context, None, None

let findNodeIdByLabel targetLbl graph nodeId (lbl, _) =
  match lbl with
  | Some (lbl) -> targetLbl = lbl
  | None -> false

let getDestination cfg nodeId =
  let blockId, stmtId = nodeId
  let _, stmt = nodeId |> getNode cfg
  match stmt with
  | CJump (_, thenLbl, elseLbl) ->
      let thenNodeId = cfg |> findNodeId (findNodeIdByLabel thenLbl)
      let elseNodeId = cfg |> findNodeId (findNodeIdByLabel elseLbl)
      [thenNodeId ; elseNodeId]
  | End (_) -> []
  | _ -> [(blockId, stmtId + 1)]

let rec execution cfg memMap (context, visited) nodeId =
  let _, stmt = nodeId |> getNode cfg
  let memMap, context, cond, addr = stmt |> executeStmt memMap context
  let visited = visited |> Set.add nodeId
  let dests = nodeId |> getDestination cfg
  match dests with
  | [ dest0 ; dest1 ] ->
      if (visited |> Set.contains dest0) || (visited |> Set.contains dest1)
      then
        raise LoopDetected
      let memMap, leftNode =
        dest0 |> execution cfg memMap (context, visited)
      let memMap, rightNode =
        dest1 |> execution cfg memMap (context, visited)
      match cond with
      | Some cond ->
          memMap, InternalNode (cond, leftNode, rightNode)
      | None -> raise ConditionNotSet
  | [ dest ] ->
      if visited |> Set.contains dest then raise LoopDetected
      dest |> execution cfg memMap (context, visited)
  | [] ->
      match addr with
      | Some addr ->
          memMap, LeafNode (context, addr)
      | None -> raise AddressNotSet
  | _ -> raise InvalidDestination

let treeToSummaryLeaf (context, _) = (* XXX *)
  context
  |> Map.fold (fun map reg expr -> map |> Map.add reg (LeafNode expr)) Map.empty

let collectReg leftSummary rightSummary =
  let leftReg =
    leftSummary |> Map.fold (fun set k _ -> set |> Set.add k) Set.empty
  let rightReg =
    rightSummary |> Map.fold (fun set k _ -> set |> Set.add k) Set.empty
  Set.union leftReg rightReg

let unionSummary leftSummary rightSummary cond summary reg =
  let leftExist = leftSummary |> Map.containsKey reg
  let rightExist = rightSummary |> Map.containsKey reg
  match leftExist, rightExist with
  | true, true ->
      let leftTree = leftSummary.Item reg
      let rightTree = rightSummary.Item reg
      summary
      |> Map.add reg (InternalNode (cond, leftTree, rightTree))
  | true, false ->
      let leftTree = leftSummary.Item reg
      let rightTree = LeafNode (Var reg)
      summary
      |> Map.add reg (InternalNode (cond, leftTree, rightTree))
  | false, true ->
      let leftTree = LeafNode (Var reg)
      let rightTree = rightSummary.Item reg
      summary
      |> Map.add reg (InternalNode (cond, leftTree, rightTree))
  | _ -> summary

let treeToSummaryInternal leftSummary rightSummary cond =
  let regs = collectReg leftSummary rightSummary
  regs |> Set.fold (unionSummary leftSummary rightSummary cond) Map.empty

let treeToSummary tree =
  tree |> treeFoldPostorder treeToSummaryLeaf treeToSummaryInternal

let findOutVar var reg =
  var = reg

let filterOutVar outVars (var : Reg) _ =
  let name, _ = var
  if name.StartsWith "mem"
  then
    true
  else
    outVars |> Set.exists (findOutVar var)

let filterMem (var : Reg) _ =
  let name, _ = var
  name.StartsWith "mem"

let getTypeFromTreeLeaf expr =
  expr |> getSizeFromExpr

let getTypeFromTreeInternal leftType rightType _ =
  if leftType = 0us || rightType = 0us
  then
    0us
  else
    leftType

let getTypeFromTree tree =
  tree |> treeFoldPostorder getTypeFromTreeLeaf getTypeFromTreeInternal

let filterUndefined _ tree =
  let ty = tree |> getTypeFromTree
  ty <> 0us

let rec optimizeMemAddrRec = function
  | UnOp (op, expr) ->
      let expr = expr |> optimizeMemAddrRec
      UnOp (op, expr)
  | BinOp (op, expr1, expr2) ->
      match op with
      | ADD ->
          let expr1, expr2 =
            match expr1, expr2 with
            | Num (_, _), _ ->
                expr2 |> optimizeMemAddrRec, expr1 |> optimizeMemAddrRec
            | expr1, BinOp (ADD, expr2, Num (value, size))
            | expr1, BinOp (ADD, Num (value, size), expr2) ->
                let expr1 = expr1 |> optimizeMemAddrRec
                let expr2 = expr2 |> optimizeMemAddrRec
                let expr1 = BinOp (ADD, expr1, expr2) |> optimizeMemAddrRec
                expr1, Num (value, size)
            | _ ->
                let expr1 = expr1 |> optimizeMemAddrRec
                let expr2 = expr2 |> optimizeMemAddrRec
                expr1, expr2
          if expr1 = expr2
          then
            let size = expr1 |> getSizeFromExpr
            BinOp (SHL, expr1, Num (1UL, size))
          else
            match expr2 with
            | Num (0UL, _) -> expr1
            | _ -> BinOp (op, expr1, expr2)
      | UMUL ->
          let expr1 = expr1 |> optimizeMemAddrRec
          let expr2 = expr2 |> optimizeMemAddrRec
          let expr1, expr2 =
            match expr1 with
            | Num (_, _) -> expr2, expr1
            | _ -> expr1, expr2
          match expr1, expr2 with
          | BinOp (SDIV, expr1, Num (32UL, 32us)), Num (4UL, 32us) ->
              BinOp (SSHR, expr1, Num (3UL, 32us))
          | expr1, Num (1UL, _) -> expr1
          | expr1, Num (2UL, _) ->
              let size = expr1 |> getSizeFromExpr
              BinOp (SHL, expr1, Num (1UL, size))
          | _ -> BinOp (op, expr1, expr2)
      | SHL
      | USHR
      | SSHR ->
          let expr1 = expr1 |> optimizeMemAddrRec
          let expr2 = expr2 |> optimizeMemAddrRec
          match expr2 with
          | Num (value, _) ->
              let size = expr1 |> getSizeFromExpr
              BinOp (op, expr1, Num (value, size))
          | _ ->
              BinOp (op, expr1, expr2)
      | _ ->
          let expr1 = expr1 |> optimizeMemAddrRec
          let expr2 = expr2 |> optimizeMemAddrRec
          BinOp (op, expr1, expr2)
  | RelOp (op, expr1, expr2) ->
      let expr1 = expr1 |> optimizeMemAddrRec
      let expr2 = expr2 |> optimizeMemAddrRec
      RelOp (op, expr1, expr2)
  | Cast (op, size, expr) ->
      let expr = expr |> optimizeMemAddrRec
      let exprSize = expr |> getSizeFromExpr
      if size = exprSize
      then
        expr
      else
        Cast (op, size, expr)
  | Ite (cond, thenExpr, elseExpr) ->
      let cond = cond |> optimizeMemAddrRec
      let thenExpr = thenExpr |> optimizeMemAddrRec
      let elseExpr = elseExpr |> optimizeMemAddrRec
      Ite (cond, thenExpr, elseExpr)
  | expr -> expr

let optimizeMemAddr map addr idx =
  let addr = addr |> optimizeMemAddrRec
  map |> Map.add addr idx

let symbolicExecution inVars outVars cfg =
  let context = Map.empty |> initContext inVars
  let memMap, tree =
    cfg
    |> findNodeId (findNodeIdByLabel "Label_Start")
    |> execution cfg Map.empty (context, Set.empty)
  // optimize memMap
  let memMap =
    memMap |> Map.fold optimizeMemAddr Map.empty
  let filteredSummary =
    tree
    |> treeToSummary
    |> Map.filter (filterOutVar outVars)
  let startTime = System.DateTime.Now
  let memSummary =
    filteredSummary
    |> Map.filter filterMem
  let curTime = System.DateTime.Now
  if (System.DateTime.Compare((startTime.AddSeconds 10.0), curTime)) = -1
  then raise TimeOut
  let regSummary =
    filteredSummary
    |> Map.filter (fun k v -> not (filterMem k v))
  let curTime = System.DateTime.Now
  if (System.DateTime.Compare((startTime.AddSeconds 10.0), curTime)) = -1
  then raise TimeOut
  let undefinedVars =
    regSummary
    |> Map.filter (fun k v -> not (filterUndefined k v))
    |> Map.toSeq
    |> Seq.map fst
    |> Set.ofSeq
  let curTime = System.DateTime.Now
  if (System.DateTime.Compare((startTime.AddSeconds 10.0), curTime)) = -1
  then raise TimeOut
  let regSummary =
    regSummary |> Map.filter filterUndefined
  let curTime = System.DateTime.Now
  if (System.DateTime.Compare((startTime.AddSeconds 10.0), curTime)) = -1
  then raise TimeOut
  (memSummary, memMap, regSummary), undefinedVars
