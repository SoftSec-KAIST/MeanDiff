module MeanDiff.Report

open Microsoft.Z3

open Alias
open CFG
open CmdOpt
open DataFlow
open JSONParse
open Lift
open SMTSolver
open Symbolic
open Tree
open Type
open UIR
open Utils

type Info = Set<Reg> * SummaryInfo

type Report =
  {
      insn : string
      arch : ArchOption
      astBAP : AST
      astPyVEX : AST
      astBINSEC : AST
      varBAP : Set<Symbol>
      varPyVEX : Set<Symbol>
      varBINSEC : Set<Symbol>
      infoBAP : Info
      infoPyVEX : Info
      infoBINSEC : Info
      undefSet : Set<Reg>
      resultBAPPYVEX : TestingResult
      resultBAPBINSEC : TestingResult
      resultPYVEXBINSEC : TestingResult
  }

let emptyReport = {
    insn = ""
    arch = X86
    astBAP = Incapable
    astPyVEX = Incapable
    astBINSEC = Incapable
    varBAP = Set.empty
    varPyVEX = Set.empty
    varBINSEC = Set.empty
    infoBAP = Set.empty, (Map.empty, Map.empty, Map.empty)
    infoPyVEX = Set.empty, (Map.empty, Map.empty, Map.empty)
    infoBINSEC = Set.empty, (Map.empty, Map.empty, Map.empty)
    undefSet = Set.empty
    resultBAPPYVEX = UNCOMPARABLE
    resultBAPBINSEC = UNCOMPARABLE
    resultPYVEXBINSEC = UNCOMPARABLE
}

let setInsnToReport insn report =
  { report with insn = insn }

let setArchToReport arch report =
  { report with arch = arch }

let translate arch json =
  let ast = json |> toAST
  ast |> typeCheck |> ignore
  ast |> alias arch

let setAstsToReport report =
  match report.arch with
  | X86 ->
      { report with
            astBAP =
              "BAP" |> lift report.arch report.insn |> translate report.arch
            astPyVEX =
              "PyVEX" |> lift report.arch report.insn |> translate report.arch
            astBINSEC =
              "BINSEC" |> lift report.arch report.insn |> translate report.arch }
  | X64 ->
      { report with
            astBAP =
              "BAP" |> lift report.arch report.insn |> translate report.arch
            astPyVEX =
              "PyVEX" |> lift report.arch report.insn |> translate report.arch }

let setVarsToReport report =
  match report.arch with
  | X86 ->
      { report with
            varBAP = "BAP" |> loadVar
            varPyVEX = "PyVEX" |> loadVar
            varBINSEC = "BINSEC" |> loadVar }
  | X64 ->
      { report with
            varBAP = "BAP" |> loadVar
            varPyVEX = "PyVEX" |> loadVar }

let summarize vars ast =
  match ast with
  | Stmts (_) ->
      let cfg = ast |> toCfg
      let inVars, outVars = cfg |> getInOutVariables vars
      let summaryInfo, undefSet = cfg |> symbolicExecution inVars outVars
      Some (inVars, summaryInfo, undefSet)
  | _ ->
      None

let setSummaryInfoToReport report =
  match report.arch with
  | X86 ->
      let inVarsBAP, summaryInfoBAP, undefBAP =
        match report.astBAP |> summarize report.varBAP with
        | Some (inVars, summaryInfo, undefSet) -> inVars, summaryInfo, undefSet
        | None -> Set.empty, (Map.empty, Map.empty, Map.empty), Set.empty
      let inVarsPyVEX, summaryInfoPyVEX, undefPyVEX =
        match report.astPyVEX |> summarize report.varPyVEX with
        | Some (inVars, summaryInfo, undefSet) -> inVars, summaryInfo, undefSet
        | None -> Set.empty, (Map.empty, Map.empty, Map.empty), Set.empty
      let inVarsBINSEC, summaryInfoBINSEC, undefBINSEC =
        match report.astBINSEC |> summarize report.varBINSEC with
        | Some (inVars, summaryInfo, undefSet) -> inVars, summaryInfo, undefSet
        | None -> Set.empty, (Map.empty, Map.empty, Map.empty), Set.empty
      let undefSet =
        seq [ undefBAP ; undefPyVEX ; undefBINSEC ] |> Set.unionMany
      { report with
            infoBAP = inVarsBAP, summaryInfoBAP
            infoPyVEX = inVarsPyVEX, summaryInfoPyVEX
            infoBINSEC = inVarsBINSEC, summaryInfoBINSEC
            undefSet = undefSet }
  | X64 ->
      let inVarsBAP, summaryInfoBAP, undefBAP =
        match report.astBAP |> summarize report.varBAP with
        | Some (inVars, summaryInfo, undefSet) -> inVars, summaryInfo, undefSet
        | None -> Set.empty, (Map.empty, Map.empty, Map.empty), Set.empty
      let inVarsPyVEX, summaryInfoPyVEX, undefPyVEX =
        match report.astPyVEX |> summarize report.varPyVEX with
        | Some (inVars, summaryInfo, undefSet) -> inVars, summaryInfo, undefSet
        | None -> Set.empty, (Map.empty, Map.empty, Map.empty), Set.empty
      let undefSet =
        seq [ undefBAP ; undefPyVEX  ] |> Set.unionMany
      { report with
            infoBAP = inVarsBAP, summaryInfoBAP
            infoPyVEX = inVarsPyVEX, summaryInfoPyVEX
            undefSet = undefSet }

let setSolverToReport report =
  match report.arch with
  | X86 ->
      let resultBAPPYVEX =
        match report.astBAP, report.astPyVEX with
        | Stmts (_), Stmts (_) ->
            differentialTesting report.undefSet report.infoBAP report.infoPyVEX
        | _ ->
            UNCOMPARABLE
      let resultBAPBINSEC =
        match report.astBAP, report.astBINSEC with
        | Stmts (_), Stmts (_) ->
            differentialTesting report.undefSet report.infoBAP report.infoBINSEC
        | _ ->
            UNCOMPARABLE
      let resultPYVEXBINSEC =
        match report.astPyVEX, report.astBINSEC with
        | Stmts (_), Stmts (_) ->
            differentialTesting report.undefSet report.infoPyVEX report.infoBINSEC
        | _ ->
            UNCOMPARABLE
      { report with
            resultBAPPYVEX = resultBAPPYVEX
            resultBAPBINSEC = resultBAPBINSEC
            resultPYVEXBINSEC = resultPYVEXBINSEC }
  | X64 ->
      let resultBAPPYVEX =
        match report.astBAP, report.astPyVEX with
        | Stmts (_), Stmts (_) ->
            differentialTesting report.undefSet report.infoBAP report.infoPyVEX
        | _ ->
            UNCOMPARABLE
      { report with
            resultBAPPYVEX = resultBAPPYVEX }

let saveReport report =
  match report.arch with
  | X86 ->
      let reportStr = ""
      let reportStr = reportStr + "======== Report ========\n"
      let reportStr =
        reportStr + (sprintf "Architecture : %s\n" (report.arch.toString))
      let reportStr =
        reportStr + (sprintf "Instruction : %s\n" report.insn)
      let reportStr =
        reportStr + (sprintf "Result between BAP and PYVEX:\n%A\n" report.resultBAPPYVEX)
      let reportStr =
        reportStr + (sprintf "Result between BAP and BINSEC:\n%A\n" report.resultBAPBINSEC)
      let reportStr =
        reportStr + (sprintf "Result between PYVEX and BINSEC:\n%A\n" report.resultPYVEXBINSEC)
      let results = [ report.resultBAPPYVEX ; report.resultBAPBINSEC ; report.resultPYVEXBINSEC ]
      let path =
        if results |> List.exists (function INEQUIV (_) -> true | _ -> false)
        then
          sprintf "reports32/inequiv/%s" report.insn
        else
          if results |> List.forall (fun x -> x = EQUIV)
          then
            sprintf "reports32/allEquiv/%s" report.insn
          else
            if results |> List.forall (fun x -> x = UNCOMPARABLE)
            then
              sprintf "reports32/allUncomp/%s" report.insn
            else
              sprintf "reports32/otherCase/%s" report.insn
      writeToFile path reportStr
  | X64 ->
      let reportStr = ""
      let reportStr = reportStr + "======== Report ========\n"
      let reportStr =
        reportStr + (sprintf "Architecture : %s\n" (report.arch.toString))
      let reportStr =
        reportStr + (sprintf "Instruction : %s\n" report.insn)
      let reportStr =
        reportStr + (sprintf "Result between BAP and PYVEX:\n%A\n" report.resultBAPPYVEX)
      let path =
        match report.resultBAPPYVEX with
        | EQUIV ->
            sprintf "reports64/equiv/%s" report.insn
        | INEQUIV (_) ->
            sprintf "reports64/inequiv/%s" report.insn
        | UNCOMPARABLE ->
            sprintf "reports64/uncomparable/%s" report.insn
      writeToFile path reportStr

let reporting option insn =
  async {
      try
        emptyReport
        |> setInsnToReport insn
        |> setArchToReport option.archOption
        |> setVarsToReport
        |> setAstsToReport
        |> setSummaryInfoToReport
        |> setSolverToReport
        |> saveReport
      with
        | IncomparableMemoryWarning ->
            let str = "Error Occured"
            let path =
              match option.archOption with
              | X86 -> sprintf "reports32/memWarn/%s" insn
              | X64 -> sprintf "reports64/memWarn/%s" insn
            writeToFile path str
        | MemoryNumberMismatch ->
            let str = "Error Occured"
            let path =
              match option.archOption with
              | X86 -> sprintf "reports32/memNumErr/%s" insn
              | X64 -> sprintf "reports64/memNumErr/%s" insn
            writeToFile path str
        | VariableNumberMismatch ->
            let str = "Error Occured"
            let path =
              match option.archOption with
              | X86 -> sprintf "reports32/varNumErr/%s" insn
              | X64 -> sprintf "reports64/varNumErr/%s" insn
            writeToFile path str
        | TimeOut ->
            let str = "Error Occured"
            let path =
              match option.archOption with
              | X86 -> sprintf "reports32/timeOutErr/%s" insn
              | X64 -> sprintf "reports64/timeOutErr/%s" insn
            writeToFile path str
        | _ ->
            let str = "Error Occured"
            let path =
              match option.archOption with
              | X86 -> sprintf "reports32/otherErr/%s" insn
              | X64 -> sprintf "reports64/otherErr/%s" insn
            writeToFile path str
  }
