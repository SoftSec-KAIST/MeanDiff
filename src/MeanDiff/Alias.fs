module MeanDiff.Alias

open CmdOpt
open UIR

let mapReg32 = function
  | "al", 8us -> ("eax", 32us)
  | "ax", 16us -> ("eax", 32us)
  | "cl", 8us -> ("ecx", 32us)
  | "cx", 16us -> ("ecx", 32us)
  | "dl", 8us -> ("edx", 32us)
  | "dx", 16us -> ("edx", 32us)
  | "bl", 8us -> ("ebx", 32us)
  | "bx", 16us -> ("ebx", 32us)
  | "sp", 16us -> ("esp", 32us)
  | "bp", 16us -> ("ebp", 32us)
  | "sil", 8us -> ("esi", 32us)
  | "si", 16us -> ("esi", 32us)
  | "dil", 8us -> ("edi", 32us)
  | "di", 16us -> ("edi", 32us)
  | "mm0", 64us -> ("ymm0", 256us)
  | "mm1", 64us -> ("ymm1", 256us)
  | "mm2", 64us -> ("ymm2", 256us)
  | "mm3", 64us -> ("ymm3", 256us)
  | "mm4", 64us -> ("ymm4", 256us)
  | "mm5", 64us -> ("ymm5", 256us)
  | "mm6", 64us -> ("ymm6", 256us)
  | "mm7", 64us -> ("ymm7", 256us)
  | "xmm0", 128us -> ("ymm0", 256us)
  | "xmm1", 128us -> ("ymm1", 256us)
  | "xmm2", 128us -> ("ymm2", 256us)
  | "xmm3", 128us -> ("ymm3", 256us)
  | "xmm4", 128us -> ("ymm4", 256us)
  | "xmm5", 128us -> ("ymm5", 256us)
  | "xmm6", 128us -> ("ymm6", 256us)
  | "xmm7", 128us -> ("ymm7", 256us)
  | reg -> reg

let mapReg64 = function
  | "al", 8us -> ("rax", 64us)
  | "ax", 16us -> ("rax", 64us)
  | "eax", 32us -> ("rax", 64us)
  | "cl", 8us -> ("rcx", 64us)
  | "cx", 16us -> ("rcx", 64us)
  | "ecx", 32us -> ("rcx", 64us)
  | "dl", 8us -> ("rdx", 64us)
  | "dx", 16us -> ("rdx", 64us)
  | "edx", 32us -> ("rdx", 64us)
  | "bl", 8us -> ("rbx", 64us)
  | "bx", 16us -> ("rbx", 64us)
  | "ebx", 32us -> ("rbx", 64us)
  | "sp", 16us -> ("rsp", 64us)
  | "esp", 32us -> ("rsp", 64us)
  | "bp", 16us -> ("rbp", 64us)
  | "ebp", 32us -> ("rbp", 64us)
  | "sil", 8us -> ("rsi", 64us)
  | "si", 16us -> ("rsi", 64us)
  | "esi", 32us -> ("rsi", 64us)
  | "dil", 8us -> ("rdi", 64us)
  | "di", 16us -> ("rdi", 64us)
  | "edi", 32us -> ("rdi", 64us)
  | "mm0", 64us -> ("ymm0", 256us)
  | "mm1", 64us -> ("ymm1", 256us)
  | "mm2", 64us -> ("ymm2", 256us)
  | "mm3", 64us -> ("ymm3", 256us)
  | "mm4", 64us -> ("ymm4", 256us)
  | "mm5", 64us -> ("ymm5", 256us)
  | "mm6", 64us -> ("ymm6", 256us)
  | "mm7", 64us -> ("ymm7", 256us)
  | "xmm0", 128us -> ("ymm0", 256us)
  | "xmm1", 128us -> ("ymm1", 256us)
  | "xmm2", 128us -> ("ymm2", 256us)
  | "xmm3", 128us -> ("ymm3", 256us)
  | "xmm4", 128us -> ("ymm4", 256us)
  | "xmm5", 128us -> ("ymm5", 256us)
  | "xmm6", 128us -> ("ymm6", 256us)
  | "xmm7", 128us -> ("ymm7", 256us)
  | reg -> reg

let mkLow size var =
  Cast (LOW, size, var)

let changeReg arch reg =
  let _, size = reg
  let mapReg =
    match arch with
    | X86 -> mapReg32
    | X64 -> mapReg64
  if reg = (reg |> mapReg)
  then
    Var reg
  else
    Var (reg |> mapReg) |> mkLow size

let rec aliasExpr arch = function
  | Var (reg) ->
      reg |> changeReg arch
  | Load (addr, size) ->
      let addr = addr |> aliasExpr arch
      Load (addr |> aliasExpr arch, size)
  | UnOp (op, expr) ->
      let expr = expr |> aliasExpr arch
      UnOp (op, expr)
  | BinOp (op, expr1, expr2) ->
      let expr1 = expr1 |> aliasExpr arch
      let expr2 = expr2 |> aliasExpr arch
      BinOp (op, expr1, expr2)
  | RelOp (op, expr1, expr2) ->
      let expr1 = expr1 |> aliasExpr arch
      let expr2 = expr2 |> aliasExpr arch
      RelOp (op, expr1, expr2)
  | Cast (op, size, expr) ->
      let expr = expr |> aliasExpr arch
      Cast (op, size, expr)
  | Ite (cond, thenExpr, elseExpr) ->
      let cond = cond |> aliasExpr arch
      let thenExpr = thenExpr |> aliasExpr arch
      let elseExpr = elseExpr |> aliasExpr arch
      Ite (cond, thenExpr, elseExpr)
  | expr -> expr

let embedExpr reg oldSize newSize expr =
  let newExpr = Cast (ZERO, newSize, expr)
  let embedding = BinOp (USHR, Var (reg), Num (oldSize |> uint64, newSize))
  let embedding = BinOp (SHL, embedding, Num (oldSize |> uint64, newSize))
  BinOp (OR, embedding, newExpr)

let aliasMove arch reg expr =
  let mapReg =
    match arch with
    | X86 -> mapReg32
    | X64 -> mapReg64
  let newReg = reg |> mapReg
  if reg = newReg
  then
    Move (reg, expr)
  else
    let _, oldSize = reg
    let _, newSize = newReg
    let expr = expr |> embedExpr newReg oldSize newSize
    Move (newReg, expr)

let aliasStmt arch = function
  | Move (reg, expr) ->
      let expr = expr |> aliasExpr arch
      aliasMove arch reg expr
  | Store (addr, expr) ->
      let addr = addr |> aliasExpr arch
      let expr = expr |> aliasExpr arch
      Store (addr, expr)
  | CJump (cond, thenLbl, elseLbl) ->
      let cond = cond |> aliasExpr arch
      CJump (cond, thenLbl, elseLbl)
  | End (addr) ->
      let addr = addr |> aliasExpr arch
      End (addr)
  | stmt -> stmt

let alias arch = function
  | Stmts (stmtList) ->
      Stmts (stmtList |> List.map (aliasStmt arch))
  | ast -> ast
