module MeanDiff.MeanDiff

open System.IO

open CmdOpt
open MultProc
open StreamGen
open Report

[<EntryPoint>]
let main argv =
  let option = parseCmdOpts argv
  try
    match option.streamOption with
    | BLACK
    | WHITE ->
        if not (File.Exists option.streamFile) then raise FileNotFound
    | _ -> ()
  with
    | FileNotFound ->
        eprintfn "File does not exist: %s" option.streamFile
        exit 1
  let agent = streamGen option |> createAgent
  [0]
  |> List.map (fun _ -> worker option agent)
  |> Async.Parallel
  |> Async.RunSynchronously
  |> ignore
  0
