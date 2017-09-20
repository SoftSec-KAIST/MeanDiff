module MeanDiff.DataFlow

open Graph
open Type
open UIR

let rec useAnalysisExpr expr useNodeSet =
  match expr with
  | Var (reg) ->
      useNodeSet
      |> Set.add reg
  | Load (addr, size) ->
      useNodeSet
      |> useAnalysisExpr addr
  | UnOp (_, expr) ->
      useNodeSet |> useAnalysisExpr expr
  | BinOp (_, expr1, expr2) ->
      useNodeSet
      |> useAnalysisExpr expr1
      |> useAnalysisExpr expr2
  | RelOp (_, expr1, expr2) ->
      useNodeSet
      |> useAnalysisExpr expr1
      |> useAnalysisExpr expr2
  | Cast (_, _, expr) ->
      useNodeSet |> useAnalysisExpr expr
  | Ite (cond, thenExpr, elseExpr) ->
      useNodeSet
      |> useAnalysisExpr cond
      |> useAnalysisExpr thenExpr
      |> useAnalysisExpr elseExpr
  | _ -> useNodeSet

let getItem (map : Map<'a, Set<'b>>) key =
  try
    map.Item key
  with
    | :? System.Collections.Generic.KeyNotFoundException -> Set.empty

let useAnalysisStmt _ useMap nodeId (_, stmt) =
  let keySet =
    match stmt with
    | Move (_, expr) ->
        Set.empty |> useAnalysisExpr expr
    | Store (addr, expr) ->
        Set.empty |> useAnalysisExpr addr |> useAnalysisExpr expr
    | CJump (cond, _, _) ->
        Set.empty |> useAnalysisExpr cond
    | End (addr) ->
        Set.empty |> useAnalysisExpr addr
    | _ -> Set.empty
  keySet
  |> Set.fold
        (fun map key -> map |> Map.add key (getItem map key |> Set.add nodeId))
        useMap

let defAnalysisStmt _ defMap nodeId (_, stmt) =
  match stmt with
  | Move (reg, _) ->
      defMap |> Map.add reg ((getItem defMap reg) |> Set.add nodeId)
  | _ -> defMap

let usedefAnalysis analysisFunc graph =
  graph |> graphFold analysisFunc Map.empty

let useAnalysis graph = usedefAnalysis useAnalysisStmt graph

let defAnalysis graph = usedefAnalysis defAnalysisStmt graph

let addDefData defData def locSet =
  if defData |> Map.exists (fun key _ -> key = def)
  then
    defData |> Map.add def (locSet |> Set.union (defData.Item def))
  else
    defData |> Map.add def locSet

let unionDefData defData1 defData2 =
  defData2 |> Map.fold addDefData defData1

let collectPredDefs graph predIdSet =
  predIdSet
  |> Set.map (getNode graph)
  |> Set.fold unionDefData Map.empty

let calcGenKill (defMap : Map<Reg, Set<'a>>) nodeId = function
  | Move (reg, _) ->
      Some (reg, nodeId), Some (reg, (defMap.Item reg |> Set.remove nodeId))
  | _ -> None, None

let removeKill kill defData =
  match kill with
  | Some (def, locSet) ->
      if defData |> Map.exists (fun key _ -> key = def)
      then
        defData |> Map.add def (locSet |> Set.difference (defData.Item def))
      else
        defData
  | None -> defData

let addGen gen defData =
  match gen with
  | Some (def, loc) ->
      if defData |> Map.exists (fun key _ -> key = def)
      then
        defData |> Map.add def ((defData.Item def) |> Set.add loc)
      else
        defData |> Map.add def (Set.empty |> Set.add loc)
  | None -> defData

let reachDefComputation defMap oldPair _ newPair nodeId (_, stmt) =
  let oldInGraph, oldOutGraph = oldPair
  let newInGraph, newOutGraph = newPair
  let newInData =
    nodeId |> getPredNodes oldOutGraph |> collectPredDefs oldOutGraph
  let newInGraph = newInGraph |> addNode nodeId newInData
  let gen, kill = calcGenKill defMap nodeId stmt
  let newOutData =
    nodeId |> getNode oldInGraph |> removeKill kill |> addGen gen
  let newOutGraph = newOutGraph |> addNode nodeId newOutData
  newInGraph, newOutGraph

let rec dataFlowRec analysisFunc graph map initGraph oldInGraph oldOutGraph =
  let func = analysisFunc map (oldInGraph, oldOutGraph)
  let newInGraph, newOutGraph =
    graph |> graphFold func (initGraph, initGraph)
  if oldInGraph = newInGraph && oldOutGraph = newOutGraph
  then
    newInGraph, newOutGraph
  else
    dataFlowRec analysisFunc graph map initGraph newInGraph newOutGraph

let dataFlowAnalysis analysisFunc initData graph map =
  let initGraph = graph |> copyGraph initData
  dataFlowRec analysisFunc graph map initGraph initGraph initGraph

let reachingDefinition defMap graph =
  dataFlowAnalysis reachDefComputation Map.empty graph defMap

let isDefined rdInGraph var inSet loc =
  if loc |> getNode rdInGraph |> Map.containsKey var
  then
    inSet
  else
    inSet |> Set.add var

let testDefined rdInGraph inSet var locSet =
  locSet |> Set.fold (isDefined rdInGraph var) inSet

let getInVariables useMap rdInGraph =
  useMap |> Map.fold (testDefined rdInGraph) Set.empty

let findVariables nameSet varSet var _ =
  let name, _ = var
  if nameSet |> Set.contains name
  then
    varSet |> Set.add var
  else
    varSet

let filterContext nameSet rdOutGraph cfg outSet nodeId (_, stmt) =
  match stmt with
  | End (_) ->
      let contextVariables =
        nodeId
        |> getNode rdOutGraph
        |> Map.fold (findVariables nameSet) Set.empty
      outSet |> Set.union contextVariables
  | _ -> outSet

let getOutVariables nameSet cfg rdOutGraph =
  cfg |> graphFold (filterContext nameSet rdOutGraph) Set.empty

let getInOutVariables nameSet cfg =
  let useMap = cfg |> useAnalysis
  let defMap = cfg |> defAnalysis
  let rdInGraph, rdOutGraph = cfg |> reachingDefinition defMap
  let inVars = getInVariables useMap rdInGraph
  let outVars = getOutVariables nameSet cfg rdOutGraph
  inVars, outVars
