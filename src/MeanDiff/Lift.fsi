module MeanDiff.Lift

open CmdOpt

val lift : ArchOption -> string -> string -> string

val loadVar : string -> Set<string>
