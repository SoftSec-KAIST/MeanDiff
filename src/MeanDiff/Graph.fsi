module MeanDiff.Graph

open UIR

type Graph<'a, 'b when 'a : comparison> =
  {
      nodes : Map<'a, 'b>
      preds : Map<'a, Set<'a>>
      succs : Map<'a, Set<'a>>
  }

val empty : Graph<'a, 'b>

val addNode : 'a -> 'b -> Graph<'a, 'b> -> Graph<'a, 'b>

val findNodeId : (Graph<'a, 'b> -> 'a -> 'b -> bool) -> Graph<'a, 'b> -> 'a

val getNode : Graph<'a, 'b> -> 'a -> 'b

val getPredNodes : Graph<'a, 'b> -> 'a -> Set<'a>

val getSuccNodes : Graph<'a, 'b> -> 'a -> Set<'a>

val connectNodes : 'a -> 'a -> Graph<'a, 'b> -> Graph<'a, 'b>

val graphMap :
  (Graph<'a, 'b> -> 'a -> 'b -> Graph<'a, 'b>) -> Graph<'a, 'b> -> Graph<'a, 'b>

val graphFold :
  (Graph<'a, 'b> -> 'c -> 'a -> 'b -> 'c) -> 'c -> Graph<'a, 'b> -> 'c

val copyGraph : 'c -> Graph<'a, 'b> -> Graph<'a, 'c>
