module MeanDiff.CFG

open Graph
open UIR

let identifyLeader (prevJump, acc) = function
  | Start (_, _, _)
  | Label (_) as stmt -> false, ((stmt, true) :: acc)
  | CJump (_, _, _)
  | End (_) as stmt -> true, ((stmt, false) :: acc)
  | stmt ->
      if prevJump = true
      then
        false, ((stmt, true) :: acc)
      else
        false, ((stmt, false) :: acc)

let cutting acc (stmt, isLeader) =
  match acc with
  | [] -> raise InvalidIRStructure
  | hd :: tl ->
      if isLeader = true
      then
        [] :: (stmt :: hd) :: tl
      else
        (stmt :: hd) :: tl

let toBlocks stmts =
  let _, aux = stmts |> List.fold identifyLeader (false, [])
  aux |> List.fold cutting [[]] |> List.filter (fun x -> x <> [])

let numbering list =
  list |> List.zip [0 .. (List.length list - 1)]

let addStmt blockId graph (stmtId, stmt) =
  let symb =
    match stmt with
    | Start (_, _, _) -> Some "Label_Start"
    | Label (symb) -> Some symb
    | _ -> None
  graph |> addNode (blockId, stmtId) (symb, stmt)

let addBlock graph (blockId, block) =
  block
  |> numbering
  |> List.fold (addStmt blockId) graph

let findNodeIdByLabel label graph nodeId (symb, _) =
  match symb with
  | Some (symb) -> label = symb
  | None -> false

let connectEdge graph nodeId (label, stmt) =
  match stmt with
  | CJump (_, thenLbl, elseLbl) ->
      let thenId = graph |> findNodeId (findNodeIdByLabel thenLbl)
      let elseId = graph |> findNodeId (findNodeIdByLabel elseLbl)
      graph
      |> connectNodes nodeId thenId
      |> connectNodes nodeId elseId
  | End (_) -> graph
  | _ ->
      let blockId, stmtId = nodeId
      graph |> connectNodes nodeId (blockId, stmtId + 1)

let toGraph stmts =
  stmts
  |> toBlocks
  |> numbering
  |> List.fold addBlock empty
  |> graphMap connectEdge

let toCfg ast =
  ast |> getStmts |> toGraph
