module MeanDiff.Utils

open System.Diagnostics
open System.IO
open System.Text.RegularExpressions

let readLinesToList = File.ReadLines >> List.ofSeq

let readLinesToSet = File.ReadLines >> Set.ofSeq

let writeListToFile lineList path =
  let lineArray = lineList |> Array.ofList
  File.WriteAllLines(path, lineArray)

let writeToFile name text = File.WriteAllText(name, text)

let isHexStr hexStr =
  let re = new Regex("^(([A-Fa-f0-9]{2})*)$")
  re.IsMatch hexStr

let toHexStr = sprintf "%02x"

let procRetValue (args : string list) =
  if List.length args <> 2 then raise OperandLengthUnmatched
  let p = new Process()
  p.StartInfo.FileName <- args.[0]
  p.StartInfo.Arguments <- args.[1]
  p.StartInfo.UseShellExecute <- false
  p.Start() |> ignore
  p.WaitForExit()
  p.ExitCode

let procStdOutStr (args : string list) =
  if List.length args <> 2 then raise OperandLengthUnmatched
  let p = new Process()
  p.StartInfo.FileName <- args.[0]
  p.StartInfo.Arguments <- args.[1]
  p.StartInfo.UseShellExecute <- false
  p.StartInfo.RedirectStandardOutput <- true
  p.Start() |> ignore
  let s = sprintf "%s" (p.StandardOutput.ReadToEnd())
  p.WaitForExit()
  s
