module MeanDiff.Lift

open CmdOpt
open Utils

let lift (arch : ArchOption) insn lifter =
  let prog = sprintf "lifters/Lifter%s/%s" lifter lifter
  let arg = sprintf "%s %s" arch.toString insn
  procStdOutStr [ prog ; arg ]

let loadVar lifter =
  let file = sprintf "lifters/Lifter%s/var%s" lifter lifter
  readLinesToSet file
