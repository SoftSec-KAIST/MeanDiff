module MeanDiff.SMTSolver

open Microsoft.Z3

open Tree
open UIR

type TestingResult =
  | EQUIV
  | INEQUIV of Set<Symbol>
  | UNCOMPARABLE

type Env = Map<Reg, BitVecExpr>

let filterMemVars ((name : Symbol), _) _ =
  name.StartsWith "mem"

let getNumMemVars summary =
  let filtered = summary |> Map.filter filterMemVars
  filtered.Count

let filterVar keySet (key : Reg) _ =
  let name, _ = key
  (name.StartsWith "mem") || (keySet |> Set.contains key)

let regToBV (ctx : Context) ((name, size) : Reg) =
  ctx.MkBVConst(name, (size |> uint32))

let calcUnOpBV (ctx : Context) bv = function
  | NEG -> ctx.MkBVNeg(bv)
  | NOT -> ctx.MkBVNot(bv)

let calcBinOpBV (ctx : Context) bv1 bv2 = function
  | ADD -> ctx.MkBVAdd(bv1, bv2)
  | SUB -> ctx.MkBVSub(bv1, bv2)
  | UMUL ->
      let bv1 = ctx.MkZeroExt(1u, bv1)
      let bv2 = ctx.MkZeroExt(1u, bv2)
      let bv = ctx.MkBVMul(bv1, bv2)
      ctx.MkExtract(bv.SortSize - 2u, 0u, bv)
  | SMUL -> ctx.MkBVMul(bv1, bv2)
  | UDIV -> ctx.MkBVUDiv(bv1, bv2)
  | SDIV -> ctx.MkBVSDiv(bv1, bv2)
  | UMOD -> ctx.MkBVURem(bv1, bv2)
  | SMOD -> ctx.MkBVSRem(bv1, bv2)
  | SHL ->
      let bv2 = ctx.MkZeroExt(bv1.SortSize - bv2.SortSize, bv2)
      ctx.MkBVSHL(bv1, bv2)
  | USHR ->
      let bv2 = ctx.MkZeroExt(bv1.SortSize - bv2.SortSize, bv2)
      ctx.MkBVLSHR(bv1, bv2)
  | SSHR ->
      let bv2 = ctx.MkZeroExt(bv1.SortSize - bv2.SortSize, bv2)
      ctx.MkBVASHR(bv1, bv2)
  | AND -> ctx.MkBVAND(bv1, bv2)
  | OR -> ctx.MkBVOR(bv1, bv2)
  | XOR -> ctx.MkBVXOR(bv1, bv2)
  | CONCAT -> ctx.MkConcat(bv1, bv2)

let calcRelOpBV (ctx : Context) bv1 bv2 op =
  let tBv = ctx.MkBV(1, 1u)
  let fBv = ctx.MkBV(0, 1u)
  match op with
  | EQ -> ctx.MkITE(ctx.MkEq(bv1, bv2), tBv, fBv) :?> BitVecExpr
  | NEQ -> ctx.MkITE(ctx.MkNot(ctx.MkEq(bv1, bv2)), tBv, fBv) :?> BitVecExpr
  | ULT -> ctx.MkITE(ctx.MkBVULT(bv1, bv2), tBv, fBv) :?> BitVecExpr
  | SLT -> ctx.MkITE(ctx.MkBVSLT(bv1, bv2), tBv, fBv) :?> BitVecExpr
  | ULE -> ctx.MkITE(ctx.MkBVULE(bv1, bv2), tBv, fBv) :?> BitVecExpr
  | SLE -> ctx.MkITE(ctx.MkBVSLE(bv1, bv2), tBv, fBv) :?> BitVecExpr

let calcCastOpBV (ctx : Context) bv size = function
  | LOW -> ctx.MkExtract(size - 1u, 0u, bv)
  | HIGH -> ctx.MkExtract(bv.SortSize - 1u, bv.SortSize - size, bv)
  | SIGN -> ctx.MkSignExt(size - bv.SortSize, bv)
  | ZERO -> ctx.MkZeroExt(size - bv.SortSize, bv)

let rec exprToBV (oldEnv : Env) (ctx : Context) (env : Env) = function
  | Num (value, size) ->
      ctx.MkBV(value, (size |> uint32)) :> BitVecExpr
  | Var (reg) ->
      oldEnv.Item reg
  | UnOp (op, expr) ->
      let bv = expr |> exprToBV oldEnv ctx env
      op |> calcUnOpBV ctx bv
  | BinOp (op, expr1, expr2) ->
      let bv1 = expr1 |> exprToBV oldEnv ctx env
      let bv2 = expr2 |> exprToBV oldEnv ctx env
      op |> calcBinOpBV ctx bv1 bv2
  | RelOp (op, expr1, expr2) ->
      let bv1 = expr1 |> exprToBV oldEnv ctx env
      let bv2 = expr2 |> exprToBV oldEnv ctx env
      op |> calcRelOpBV ctx bv1 bv2
  | Ite (cond, thenExpr, elseExpr) ->
      let condBv = cond |> exprToBV oldEnv ctx env
      let condBv = ctx.MkEq(condBv, ctx.MkBV(1, 1u))
      let thenBv = thenExpr |> exprToBV oldEnv ctx env
      let elseBv = elseExpr |> exprToBV oldEnv ctx env
      ctx.MkITE(condBv, thenBv, elseBv) :?> BitVecExpr
  | Cast (op, size, expr) ->
      let bv = expr |> exprToBV oldEnv ctx env
      op |> calcCastOpBV ctx bv (size |> uint32)
  | expr -> raise NotImplemented

let addVarToEnv ctx env reg =
  env |> Map.add reg (reg |> regToBV ctx)

let initEnv (ctx : Context) inVars =
  inVars |> Set.fold (addVarToEnv ctx) Map.empty

let treeToExprLeaf oldEnv ctx env expr =
  expr |> exprToBV oldEnv ctx env

let treeToExprInternal oldEnv (ctx : Context) env leftExpr rightExpr cond =
  let condBV = ctx.MkEq(cond |> exprToBV oldEnv ctx env, ctx.MkBV(1, 1u))
  ctx.MkITE(condBV, leftExpr, rightExpr) :?> BitVecExpr

let treeToExpr oldEnv ctx env tree =
  let leafFunc = treeToExprLeaf oldEnv ctx env
  let internalFunc = treeToExprInternal oldEnv ctx env
  tree |> treeFoldPostorder leafFunc internalFunc

let convertToBV oldEnv (ctx : Context) env (reg : Reg) tree =
  let name, size = reg
  let value = tree |> treeToExpr oldEnv ctx env
  env |> Map.add reg value

let memToVar summary varSet _ idx =
  let name = sprintf "mem%d" idx
  let reg = summary |> Map.findKey (fun (name_, _) _ -> name_ = name)
  varSet |> Set.add reg

let convertToEnv (ctx : Context) keys info =
  let inVars, memSummary, regSummary = info
  let vars =
    memSummary |> Map.toSeq |> Seq.map fst |> Set.ofSeq |> Set.union inVars
  let env = vars |> initEnv ctx
  let summary =
    Map(Seq.concat [ (Map.toSeq memSummary) ; (Map.toSeq regSummary) ])
  summary
  |> Map.fold (convertToBV env ctx) env
  |> Map.filter (filterVar keys)

let combineEnv (ctx:Context) (env2:Map<Reg, BitVecExpr>) acc (key1:Reg) value1 =
  let name1, size1 = key1
  if name1.StartsWith("mem")
  then
    let name2, size2 = env2 |> Map.findKey (fun (name, _) _ -> name = name1)
    if size1 = size2
    then
      acc |> Map.add key1 (value1, env2.Item key1)
    else
      let value2 = env2.Item (name2, size2)
      let bigSize, smallSize =
        if size1 > size2 then size1, size2 else size2, size1
      let bigValue, smallValue =
        if size1 > size2 then value1, value2 else value2, value1
      let size =
        ctx.MkBV((smallSize |> uint32), (bigSize |> uint32)) :> BitVecExpr
      let orig = ctx.MkBVConst(name1, bigSize |> uint32)
      let orig = ctx.MkBVLSHR(orig, size)
      let orig = ctx.MkBVSHL(orig, size)
      let smallValue =
        ctx.MkZeroExt((bigSize - smallSize) |> uint32, smallValue)
      let smallValue = ctx.MkBVOR(orig, smallValue)
      acc |> Map.add key1 (bigValue, smallValue)
  else
    acc |> Map.add key1 (value1, env2.Item key1)

let equating (ctx : Context) acc _ (e1, e2) =
  ctx.MkOr(acc, ctx.MkNot(ctx.MkEq(e1, e2)))

let mkEquation (ctx : Context) env1 env2 =
  let combined = env1 |> Map.fold (combineEnv ctx env2) Map.empty
  let acc = ctx.MkFalse()
  let equation =
    combined
    |> Map.fold (equating ctx) acc
  combined, equation

let renameMemFold (memMap1 : Map<Expr, int>) memSummary2 map addr idx =
  let name = sprintf "mem%d" (memMap1.Item addr)
  let oldname = sprintf "mem%d" idx
  let _, size =
    memSummary2 |> Map.findKey (fun (name, size) _ -> name = oldname)
  let expr = memSummary2.Item (oldname, size)
  map |> Map.add (oldname, size) expr

let renameMem memMap1 memMap2 memSummary2 =
  memMap2 |> Map.fold (renameMemFold memMap1 memSummary2) Map.empty

let differentialTesting undefSet info1 info2 =
  let inVars1, (memSummary1, memMap1, regSummary1) = info1
  let inVars2, (memSummary2, memMap2, regSummary2) = info2

  let memKey1 = memMap1 |> Map.toSeq |> Seq.map fst |> Set.ofSeq
  let memKey2 = memMap2 |> Map.toSeq |> Seq.map fst |> Set.ofSeq

  printfn "%A" memKey1
  printfn "%A" memKey2
  if (memKey1 |> Set.count) <> (memKey2 |> Set.count)
  then raise MemoryNumberMismatch

  if memKey1 <> memKey2
  then raise IncomparableMemoryWarning

  let memSummary2 = memSummary2 |> renameMem memMap1 memMap2

  let regKey1 =
    regSummary1
    |> Map.toSeq
    |> Seq.map fst
    |> Set.ofSeq
    |> Set.filter (fun key -> undefSet |> Set.contains key |> not)
  let regKey2 =
    regSummary2
    |> Map.toSeq
    |> Seq.map fst
    |> Set.ofSeq
    |> Set.filter (fun key -> undefSet |> Set.contains key |> not)

  (*
  if regKey1 <> regKey2
  then raise VariableNumberMismatch
  *)
  let regKey = Set.union regKey1 regKey2

  let regSummary1 =
    regSummary1
    |> Map.filter (fun key _ -> regKey |> Set.contains key)
  let regSummary2 =
    regSummary2
    |> Map.filter (fun key _ -> regKey |> Set.contains key)

  let ctx = new Context()

  let env1 = (inVars1, memSummary1, regSummary1) |> convertToEnv ctx regKey1
  let env2 = (inVars2, memSummary2, regSummary2) |> convertToEnv ctx regKey2

  let combined, equation = mkEquation ctx env1 env2
  let solver = ctx.MkSolver()
  solver.Assert(equation)
  match solver.Check() with
  | Status.UNSATISFIABLE -> EQUIV
  | Status.SATISFIABLE ->
      let model = solver.Model
      printfn "%A" model
      let regs =
        combined
        |> Map.map (fun k (e1, e2) -> printfn "%A - %A - %A" k (model.Eval(e1)) (model.Eval(e2)) ; model.Eval(e1) = model.Eval(e2))
        |> Map.filter (fun _ v -> v = false)
        |> Map.toSeq
        |> Seq.map fst
        |> Seq.map fst
        |> Set.ofSeq
      INEQUIV (regs)
  | _ -> raise SolverUnknown
