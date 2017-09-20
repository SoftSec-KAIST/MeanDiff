module MeanDiff.Graph

open UIR

type Graph<'a, 'b when 'a : comparison> =
  {
      nodes : Map<'a, 'b>
      preds : Map<'a, Set<'a>>
      succs : Map<'a, Set<'a>>
  }

let empty = { nodes = Map.empty ; preds = Map.empty ; succs = Map.empty }

let addNode nodeId nodeData graph =
  { graph with nodes = graph.nodes |> Map.add nodeId nodeData }

let findNodeId findFunc graph =
  graph.nodes |> Map.findKey (findFunc graph)

let getNode graph nodeId = graph.nodes.Item nodeId

let getPredNodes graph nodeId =
  if graph.preds |> Map.containsKey nodeId
  then
    graph.preds.Item nodeId
  else
    Set.empty

let getSuccNodes graph nodeId =
  if graph.succs |> Map.containsKey nodeId
  then
    graph.succs.Item nodeId
  else
    Set.empty

let connectNodes srcId dstId graph =
  { graph with
        preds = graph.preds
                |> Map.add dstId (dstId |> getPredNodes graph |> Set.add srcId)
        succs = graph.succs
                |> Map.add srcId (srcId |> getSuccNodes graph |> Set.add dstId)
  }

let graphMap mapFunc graph = graph.nodes |> Map.fold mapFunc graph

let graphFold foldFunc acc graph = graph.nodes |> Map.fold (foldFunc graph) acc

let copyGraphNode initData graph copied nodeId nodeData =
  copied |> addNode nodeId initData

let copyGraphEdge graph copied nodeId nodeData =
  nodeId
  |> getSuccNodes graph
  |> Set.fold (fun graph dstId -> graph |> connectNodes nodeId dstId) copied

let copyGraph initData graph =
  let copied = graph |> graphFold (copyGraphNode initData) empty
  graph |> graphFold copyGraphEdge copied
