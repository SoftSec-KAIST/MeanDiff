module MeanDiff.CFG

open Graph
open UIR

val toCfg : AST -> Graph<int * int, Symbol option * Stmt>
