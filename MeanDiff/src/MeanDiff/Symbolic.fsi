module MeanDiff.Symbolic

open Graph
open Tree
open UIR

type Summary = Map<Reg, Tree<Expr, Expr>>

type SummaryInfo = Summary * Map<Expr, int> * Summary

val symbolicExecution :
  Set<Reg>
  -> Set<Reg>
  -> Graph<('c * int), (string option * Stmt)>
  -> SummaryInfo * Set<Reg>
