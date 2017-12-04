module MeanDiff.CmdOpt

type ArchOption =
  | X86
  | X64
with
  member toString : string

type StreamOption =
  | ALL
  | BLACK
  | WHITE
  | USE

type CmdOpts =
  {
      // If this flag is true, arch option is already parsed
      archFlag : bool
      // Indicates target architecture
      archOption : ArchOption
      // If this flag is true, help msg should be printed
      helpFlag : bool
      // If this flag is true, stream-mode option is already parsed
      streamFlag : bool
      // Indicates stream generating mode
      streamOption : StreamOption
      // Indicates a list for stream generating
      streamFile : string
  }

type Mode =
  | TopLevel
  | ArchParse
  | StreamParse
  | StreamFile

val parseCmdOpts : string [] -> CmdOpts
