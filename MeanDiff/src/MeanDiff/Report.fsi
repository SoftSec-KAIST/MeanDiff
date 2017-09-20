module MeanDiff.Report

open CmdOpt

val reporting : CmdOpts -> string -> Async<unit>
