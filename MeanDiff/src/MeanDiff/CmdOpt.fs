module MeanDiff.CmdOpt

type ArchOption =
  | X86
  | X64
with
  member this.toString =
    match this with
    | X86 -> "x86"
    | X64 -> "x64"

type StreamOption =
  | ALL
  | BLACK
  | WHITE
  | USE

type CmdOpts =
  {
      archFlag : bool
      archOption : ArchOption
      helpFlag : bool
      streamFlag : bool
      streamOption : StreamOption
      streamFile : string
  }

type Mode =
  | TopLevel
  | ArchParse
  | StreamParse
  | StreamFile

type FoldState =
  {
      opts : CmdOpts
      mode : Mode
  }

let parseTopLevel options = function
  | "--arch" ->
      if not options.archFlag
      then
        let options = { options with archFlag = true }
        { opts = options ; mode = ArchParse }
      else
        raise DuplicatedOption
  | "--help" ->
      if not options.helpFlag
      then
        let options = { options with helpFlag = true }
        { opts = options ; mode = TopLevel }
      else
        raise DuplicatedOption
  | "--stream" ->
      if not options.streamFlag
      then
        let options = { options with streamFlag = true }
        { opts = options ; mode = StreamParse }
      else
        raise DuplicatedOption
  | arg ->
      raise InvalidOption

let parseArch options arg =
  let options =
    match arg with
    | "x86" -> { options with archOption = X86 }
    | "x64" -> { options with archOption = X64 }
    | _ -> raise InvalidOption
  { opts = options ; mode = TopLevel }

let parseStream options = function
  | "All" ->
      { opts = { options with streamOption = ALL } ; mode = TopLevel }
  | "BlackList" ->
      { opts = { options with streamOption = BLACK } ; mode = StreamFile }
  | "WhiteList" ->
      { opts = { options with streamOption = WHITE } ; mode = StreamFile }
  | "Use" ->
      { opts = { options with streamOption = USE } ; mode = TopLevel }
  | arg -> raise InvalidOption

let parseStreamFile options arg =
  { opts = { options with streamFile = arg } ; mode = TopLevel }

let parse arg = function
  | { opts = options ; mode = TopLevel } ->
      parseTopLevel options arg
  | { opts = options ; mode = ArchParse } ->
      parseArch options arg
  | { opts = options ; mode = StreamParse } ->
      parseStream options arg
  | { opts = options ; mode = StreamFile } ->
      parseStreamFile options arg

let help () =
  printfn "[Usage] MeanDiff.exe [Options...]"

let parseCmdOpts args =
  if Array.length args < 1
  then
    help ()
    exit 1

  let defaultOpts = {
      archFlag = false
      archOption = X86
      helpFlag = false
      streamFlag = false
      streamOption = ALL
      streamFile = ""
  }
  let initialState = { opts = defaultOpts ; mode = TopLevel }

  try
    let { opts = options ; mode = mode } =
      Array.fold (fun state arg -> parse arg state) initialState args
    if mode <> TopLevel then raise InsufficientOption
    if options.helpFlag
    then
      help ()
      exit 1
    else
      options
  with
    | InsufficientOption ->
        eprintfn "Insufficient Option: %s" (String.concat " " args) |> help
        exit 1
    | InvalidOption ->
        eprintfn "Invalid Option: %s" (String.concat " " args) |> help
        exit 1
    | DuplicatedOption ->
        eprintfn "Duplicated Option: %s" (String.concat " " args) |> help
        exit 1
