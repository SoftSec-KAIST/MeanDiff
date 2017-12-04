module MeanDiff.Lift

open CmdOpt
open Utils

let lift (arch : ArchOption) insn lifter =
  let prog = sprintf "build/Lifter%s" lifter
  let arg = sprintf "%s %s" arch.toString insn
  procStdOutStr [ prog ; arg ]

let loadVar lifter =
  let file = sprintf "build/Lifter%s" lifter
  readLinesToSet file
