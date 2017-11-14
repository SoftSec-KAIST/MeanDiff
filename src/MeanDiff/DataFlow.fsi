module MeanDiff.DataFlow

open Graph
open UIR

val getInOutVariables :
  Set<Symbol>
  -> Graph<'a, 'b * Stmt>
  -> Set<Reg> * Set<Reg>
