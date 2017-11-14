from .Errors import *
from .UIR import *

def Number(n, size):
    return Num(Basic(n), Basic(size))

def Not(expr):
    op = NOT()
    return UnOp(op, expr)

def Add(expr1, expr2):
    op = ADD()
    return BinOp(op, expr1, expr2)

def Sub(expr1, expr2):
    op = SUB()
    return BinOp(op, expr1, expr2)

def Smul(expr1, expr2):
    op = SMUL()
    return BinOp(op, expr1, expr2)

def Umul(expr1, expr2):
    op = UMUL()
    return BinOp(op, expr1, expr2)

def Shl(expr1, expr2):
    op = SHL()
    return BinOp(op, expr1, expr2)

def Shl_n(n, size, expr):
    op = SHL()
    num = Number(n, size)
    return BinOp(op, expr, num)

def Sshr(expr1, expr2):
    op = SSHR()
    return BinOp(op, expr1, expr2)

def Sshr_n(n, size, expr):
    op = SSHR()
    num = Number(n, size)
    return BinOp(op, expr, num)

def Ushr(expr1, expr2):
    op = USHR()
    return BinOp(op, expr1, expr2)

def Ushr_n(n, size, expr):
    op = USHR()
    num = Number(n, size)
    return BinOp(op, expr, num)

def And(expr1, expr2):
    op = AND()
    return BinOp(op, expr1, expr2)

def Or(expr1, expr2):
    op = OR()
    return BinOp(op, expr1, expr2)

def Xor(expr1, expr2):
    op = XOR()
    return BinOp(op, expr1, expr2)

def Concat(expr1, expr2):
    op = CONCAT()
    return BinOp(op, expr1, expr2)

def Eq(expr1, expr2):
    op = EQ()
    return RelOp(op, expr1, expr2)

def Neq(expr1, expr2):
    op = NEQ()
    return RelOp(op, expr1, expr2)

def Slt(expr1, expr2):
    op = SLT()
    return RelOp(op, expr1, expr2)

def Ule(expr1, expr2):
    op = ULE()
    return RelOp(op, expr1, expr2)

def Ult(expr1, expr2):
    op = ULT()
    return RelOp(op, expr1, expr2)

def Low(size, expr):
    op = LOW()
    size = Basic(size)
    return Cast(op, size, expr)

def High(size, expr):
    op = HIGH()
    size = Basic(size)
    return Cast(op, size, expr)

def Sign(size, expr):
    op = SIGN()
    size = Basic(size)
    return Cast(op, size, expr)

def Zero(size, expr):
    op = ZERO()
    size = Basic(size)
    return Cast(op, size, expr)

################ Helpers for SIMD ################

def mk8x8(expr7, expr6, expr5, expr4, expr3, expr2, expr1, expr0):
    expr7 = Shl_n(56, 64, Zero(64, expr7))
    expr6 = Shl_n(48, 64, Zero(64, expr6))
    expr5 = Shl_n(40, 64, Zero(64, expr5))
    expr4 = Shl_n(32, 64, Zero(64, expr4))
    expr3 = Shl_n(24, 64, Zero(64, expr3))
    expr2 = Shl_n(16, 64, Zero(64, expr2))
    expr1 = Shl_n(8, 64, Zero(64, expr1))
    expr0 = Zero(64, expr0)
    hi = Or(Or(expr7, expr6), Or(expr5, expr4))
    lo = Or(Or(expr3, expr2), Or(expr1, expr0))
    return Or(hi, lo)

def mk16x4(expr3, expr2, expr1, expr0):
    expr3 = Shl_n(48, 64, Zero(64, expr3))
    expr2 = Shl_n(32, 64, Zero(64, expr2))
    expr1 = Shl_n(16, 64, Zero(64, expr1))
    expr0 = Zero(64, expr0)
    return Or(Or(expr3, expr2), Or(expr1, expr0))

def mk32x2(expr1, expr0):
    expr1 = Shl_n(32, 64, Zero(64, expr1))
    expr0 = Zero(64, expr0)
    return Or(expr1, expr0)

def mk8x16(expr15, expr14, expr13, expr12, expr11, expr10, expr9, expr8,
           expr7, expr6, expr5, expr4, expr3, expr2, expr1, expr0):
    expr15 = Shl_n(120, 128, Zero(128, expr15))
    expr14 = Shl_n(112, 128, Zero(128, expr14))
    expr13 = Shl_n(104, 128, Zero(128, expr13))
    expr12 = Shl_n(96, 128, Zero(128, expr12))
    expr11 = Shl_n(88, 128, Zero(128, expr11))
    expr10 = Shl_n(80, 128, Zero(128, expr10))
    expr9 = Shl_n(72, 128, Zero(128, expr9))
    expr8 = Shl_n(64, 128, Zero(128, expr8))
    expr7 = Shl_n(56, 128, Zero(128, expr7))
    expr6 = Shl_n(48, 128, Zero(128, expr6))
    expr5 = Shl_n(40, 128, Zero(128, expr5))
    expr4 = Shl_n(32, 128, Zero(128, expr4))
    expr3 = Shl_n(24, 128, Zero(128, expr3))
    expr2 = Shl_n(16, 128, Zero(128, expr2))
    expr1 = Shl_n(8, 128, Zero(128, expr1))
    expr0 = Zero(128, expr0)
    hi1 = Or(Or(expr15, expr14), Or(expr13, expr12))
    hi0 = Or(Or(expr11, expr10), Or(expr9, expr8))
    lo1 = Or(Or(expr7, expr6), Or(expr5, expr4))
    lo0 = Or(Or(expr3, expr2), Or(expr1, expr0))
    return Or(Or(hi1, hi0), Or(lo1, lo0))

def mk16x8(expr7, expr6, expr5, expr4, expr3, expr2, expr1, expr0):
    expr7 = Shl_n(112, 128, Zero(128, expr7))
    expr6 = Shl_n(96, 128, Zero(128, expr6))
    expr5 = Shl_n(80, 128, Zero(128, expr5))
    expr4 = Shl_n(64, 128, Zero(128, expr4))
    expr3 = Shl_n(48, 128, Zero(128, expr3))
    expr2 = Shl_n(32, 128, Zero(128, expr2))
    expr1 = Shl_n(16, 128, Zero(128, expr1))
    expr0 = Zero(128, expr0)
    hi = Or(Or(expr7, expr6), Or(expr5, expr4))
    lo = Or(Or(expr3, expr2), Or(expr1, expr0))
    return Or(hi, lo)

def mk32x4(expr3, expr2, expr1, expr0):
    expr3 = Shl_n(96, 128, Zero(128, expr3))
    expr2 = Shl_n(64, 128, Zero(128, expr2))
    expr1 = Shl_n(32, 128, Zero(128, expr1))
    expr0 = Zero(128, expr0)
    return Or(Or(expr3, expr2), Or(expr1, expr0))

def mk64x2(expr1, expr0):
    expr1 = Shl_n(64, 128, Zero(128, expr1))
    expr0 = Zero(128, expr0)
    return Or(expr1, expr0)

def sel8x8_7(expr):
    expr = Ushr_n(56, 64, expr)
    return Low(8, expr)

def sel8x8_6(expr):
    expr = Ushr_n(48, 64, expr)
    return Low(8, expr)

def sel8x8_5(expr):
    expr = Ushr_n(40, 64, expr)
    return Low(8, expr)

def sel8x8_4(expr):
    expr = Ushr_n(32, 64, expr)
    return Low(8, expr)

def sel8x8_3(expr):
    expr = Ushr_n(24, 64, expr)
    return Low(8, expr)

def sel8x8_2(expr):
    expr = Ushr_n(16, 64, expr)
    return Low(8, expr)

def sel8x8_1(expr):
    expr = Ushr_n(8, 64, expr)
    return Low(8, expr)

def sel8x8_0(expr):
    return Low(8, expr)

def sel16x4_3(expr):
    expr = Ushr_n(48, 64, expr)
    return Low(16, expr)

def sel16x4_2(expr):
    expr = Ushr_n(32, 64, expr)
    return Low(16, expr)

def sel16x4_1(expr):
    expr = Ushr_n(16, 64, expr)
    return Low(16, expr)

def sel16x4_0(expr):
    return Low(16, expr)

def sel32x2_1(expr):
    expr = Ushr_n(32, 64, expr)
    return Low(32, expr)

def sel32x2_0(expr):
    return Low(32, expr)

def sel8x16_15(expr):
    expr = Ushr_n(120, 128, expr)
    return Low(8, expr)

def sel8x16_14(expr):
    expr = Ushr_n(112, 128, expr)
    return Low(8, expr)

def sel8x16_13(expr):
    expr = Ushr_n(104, 128, expr)
    return Low(8, expr)

def sel8x16_12(expr):
    expr = Ushr_n(96, 128, expr)
    return Low(8, expr)

def sel8x16_11(expr):
    expr = Ushr_n(88, 128, expr)
    return Low(8, expr)

def sel8x16_10(expr):
    expr = Ushr_n(80, 128, expr)
    return Low(8, expr)

def sel8x16_9(expr):
    expr = Ushr_n(72, 128, expr)
    return Low(8, expr)

def sel8x16_8(expr):
    expr = Ushr_n(64, 128, expr)
    return Low(8, expr)

def sel8x16_7(expr):
    expr = Ushr_n(56, 128, expr)
    return Low(8, expr)

def sel8x16_6(expr):
    expr = Ushr_n(48, 128, expr)
    return Low(8, expr)

def sel8x16_5(expr):
    expr = Ushr_n(40, 128, expr)
    return Low(8, expr)

def sel8x16_4(expr):
    expr = Ushr_n(32, 128, expr)
    return Low(8, expr)

def sel8x16_3(expr):
    expr = Ushr_n(24, 128, expr)
    return Low(8, expr)

def sel8x16_2(expr):
    expr = Ushr_n(16, 128, expr)
    return Low(8, expr)

def sel8x16_1(expr):
    expr = Ushr_n(8, 128, expr)
    return Low(8, expr)

def sel8x16_0(expr):
    return Low(8, expr)

def sel16x8_7(expr):
    expr = Ushr_n(112, 128, expr)
    return Low(16, expr)

def sel16x8_6(expr):
    expr = Ushr_n(96, 128, expr)
    return Low(16, expr)

def sel16x8_5(expr):
    expr = Ushr_n(80, 128, expr)
    return Low(16, expr)

def sel16x8_4(expr):
    expr = Ushr_n(64, 128, expr)
    return Low(16, expr)

def sel16x8_3(expr):
    expr = Ushr_n(48, 128, expr)
    return Low(16, expr)

def sel16x8_2(expr):
    expr = Ushr_n(32, 128, expr)
    return Low(16, expr)

def sel16x8_1(expr):
    expr = Ushr_n(16, 128, expr)
    return Low(16, expr)

def sel16x8_0(expr):
    return Low(16, expr)

def sel32x4_3(expr):
    expr = Ushr_n(96, 128, expr)
    return Low(32, expr)

def sel32x4_2(expr):
    expr = Ushr_n(64, 128, expr)
    return Low(32, expr)

def sel32x4_1(expr):
    expr = Ushr_n(32, 128, expr)
    return Low(32, expr)

def sel32x4_0(expr):
    return Low(32, expr)

def sel64x2_1(expr):
    expr = Ushr_n(64, 128, expr)
    return Low(64, expr)

def sel64x2_0(expr):
    return Low(64, expr)

def mk64(size, expr):
    if size == 8:
        return mk8x8(expr[7], expr[6], expr[5], expr[4],
                     expr[3], expr[2], expr[1], expr[0])
    elif size == 16:
        return mk16x4(expr[3], expr[2], expr[1], expr[0])
    elif size == 32:
        return mk32x2(expr[1], expr[0])

def mk128(size, expr):
    if size == 8:
        return mk8x16(expr[15], expr[14], expr[13], expr[12],
                      expr[11], expr[10], expr[9], expr[8],
                      expr[7], expr[6], expr[5], expr[4],
                      expr[3], expr[2], expr[1], expr[0])
    elif size == 16:
        return mk16x8(expr[7], expr[6], expr[5], expr[4],
                      expr[3], expr[2], expr[1], expr[0])
    elif size == 32:
        return mk32x4(expr[3], expr[2], expr[1], expr[0])
    elif size == 64:
        return mk64x2(expr[1], expr[0])

def sel64(size, expr):
    if size == 8:
        return [sel8x8_7(expr), sel8x8_6(expr), sel8x8_5(expr), sel8x8_4(expr),
                sel8x8_3(expr), sel8x8_2(expr), sel8x8_1(expr), sel8x8_0(expr)]
    elif size == 16:
        return [sel16x4_3(expr), sel16x4_2(expr),
                sel16x4_1(expr), sel16x4_0(expr)]
    elif size == 32:
        return [sel32x2_1(expr), sel32x2_0(expr)]

def sel128(size, expr):
    if size == 8:
        return [sel8x16_15(expr), sel8x16_14(expr), sel8x16_13(expr), sel8x16_12(expr),
                sel8x16_11(expr), sel8x16_10(expr), sel8x16_9(expr), sel8x16_8(expr),
                sel8x16_7(expr), sel8x16_6(expr), sel8x16_5(expr), sel8x16_4(expr),
                sel8x16_3(expr), sel8x16_2(expr), sel8x16_1(expr), sel8x16_0(expr)]
    elif size == 16:
        return [sel16x8_7(expr), sel16x8_6(expr), sel16x8_5(expr), sel16x8_4(expr),
                sel16x8_3(expr), sel16x8_2(expr), sel16x8_1(expr), sel16x8_0(expr)]
    elif size == 32:
        return [sel32x4_3(expr), sel32x4_2(expr), sel32x4_1(expr), sel32x4_0(expr)]
    elif size == 64:
        return [sel64x2_1(expr), sel64x2_0(expr)]

################ Helpers for Operations ################

def qopS(func, size, expr1, expr2):
    expr1 = Sign(size * 2, expr1)
    expr2 = Sign(size * 2, expr2)
    expr = func(expr1, expr2)
    cond = Slt(Number(1 << (size - 1) - 1, size * 2), expr)
    ite = Ite(cond, Number(1 << (size - 1) - 1, size * 2), expr)
    cond = Slt(expr, Number(1 << (size - 1), size * 2))
    ite = Ite(cond, Number(1 << (size - 1), size * 2), ite)
    return Low(size, ite)

def qopU(func, size, expr1, expr2):
    expr1 = Zero(size * 2, expr1)
    expr2 = Zero(size * 2, expr2)
    expr = func(expr1, expr2)
    cond = Ult(Number(1 << size - 1, size * 2), expr)
    ite = Ite(cond, Number(1 << size - 1, size * 2), expr)
    return Low(size, ite)

def qadd8S(expr1, expr2):
    return qopS(Add, 8, expr1, expr2)

def qadd8U(expr1, expr2):
    return qopU(Add, 8, expr1, expr2)

def qadd16S(expr1, expr2):
    return qopS(Add, 16, expr1, expr2)

def qadd16U(expr1, expr2):
    return qopU(Add, 16, expr1, expr2)

def qsub8S(expr1, expr2):
    return qopS(Sub, 8, expr1, expr2)

def qsub8U(expr1, expr2):
    return qopU(Sub, 8, expr1, expr2)

def qsub16S(expr1, expr2):
    return qopS(Sub, 16, expr1, expr2)

def qsub16U(expr1, expr2):
    return qopU(Sub, 16, expr1, expr2)

def mulhi16S(expr1, expr2):
    expr1 = Sign(32, expr1)
    expr2 = Sign(32, expr2)
    expr = Smul(expr1, expr2)
    expr = Sshr_n(16, 32, expr)
    return Low(16, expr)

def mulhi16U(expr1, expr2):
    expr1 = Zero(32, expr1)
    expr2 = Zero(32, expr2)
    expr = Umul(expr1, expr2)
    expr = Ushr_n(16, 32, expr)
    return Low(16, expr)

def index8x8(expr1, expr2):
    expr = Ushr(expr1, expr2)
    return Low(8, expr)

def cmp(func, size, expr1, expr2):
    cond = func(expr1, expr2)
    return Ite(cond, Number(1 << size - 1, size), Number(0, size))

def cmpeq8(expr1, expr2):
    return cmp(Eq, 8, expr1, expr2)

def cmpeq16(expr1, expr2):
    return cmp(Eq, 16, expr1, expr2)

def cmpeq32(expr1, expr2):
    return cmp(Eq, 32, expr1, expr2)

def cmpgt8S(expr1, expr2):
    return cmp(Slt, 8, expr2, expr1)

def cmpgt16S(expr1, expr2):
    return cmp(Slt, 16, expr2, expr1)

def cmpgt32S(expr1, expr2):
    return cmp(Slt, 32, expr2, expr1)

def minmax(func, is_min, expr1, expr2):
    cond = func(expr1, expr2)
    if is_min:
        return Ite(cond, expr1, expr2)
    else:
        return Ite(cond, expr2, expr1)

def max8U(expr1, expr2):
    return minmax(Ult, False, expr2, expr1)

def max16S(expr1, expr2):
    return minmax(Slt, False, expr2, expr1)

def min8U(expr1, expr2):
    return minmax(Ult, True, expr1, expr2)

def min16S(expr1, expr2):
    return minmax(Slt, True, expr1, expr2)

def avgU(size, expr1, expr2):
    expr1 = Zero(size * 2, expr1)
    expr2 = Zero(size * 2, expr2)
    expr = Add(Add(expr1, expr2), Number(1, size * 2))
    expr = Ushr_n(1, size * 2, expr)
    return Low(size, expr)

def avg8U(expr1, expr2):
    return avgU(8, expr1, expr2)

def avg16U(expr1, expr2):
    return avgU(16, expr1, expr2)

def qnarrowS(size, expr):
    cond = Slt(Number(1 << (size - 1) - 1, size * 2), expr)
    ite = Ite(cond, Number(1 << (size - 1) - 1, size * 2), expr)
    cond = Slt(expr, Number(1 << (size - 1), size * 2))
    ite = Ite(cond, Number(1 << (size - 1), size * 2), ite)
    return Low(size, ite)

def qnarrowU(size, expr):
    cond = Slt(Number(1 << size - 1, size * 2), expr)
    ite = Ite(cond, Number(1 << size - 1, size * 2), expr)
    cond = Slt(expr, Number(0, size * 2))
    ite = Ite(cond, Number(0, size * 2), ite)
    return Low(size, ite)

def qnarrow16Sto8S(expr):
    return qnarrowS(8, expr)

def qnarrow16Sto8U(expr):
    return qnarrowU(8, expr)

def qnarrow32Sto16S(expr):
    return qnarrowS(16, expr)

################ Helpers for Unary Operations ################

def Clz32(expr):
    cond = Neq(And(expr, Number(1 << 1, 32)), Number(0, 32))
    ite = Ite(cond, Number(30, 32), Number(31, 32))
    for i in range(30)[::-1]:
        cond = Neq(And(expr, Number(1 << (31 - i), 32)), Number(0, 32))
        ite = Ite(cond, Number(i, 32), ite)
    return ite

def Clz64(expr):
    cond = Neq(And(expr, Number(1 << 1, 64)), Number(0, 64))
    ite = Ite(cond, Number(62, 64), Number(63, 64))
    for i in range(62)[::-1]:
        cond = Neq(And(expr, Number(1 << (63 - i), 64)), Number(0, 64))
        ite = Ite(cond, Number(i, 64), ite)
    return ite

def Ctz32(expr):
    cond = Neq(And(expr, Number(1 << 31, 32)), Number(0, 32))
    ite = Ite(cond, Num(Basic(30), Basic(32)), Num(Basic(31), Basic(32)))
    for i in range(30)[::-1]:
        cond = Neq(And(expr, Number(1 << i, 32)), Number(0, 32))
        ite = Ite(cond, Number(i, 32), ite)
    return ite

def Ctz64(expr):
    cond = Neq(And(expr, Number(1 << 63, 64)), Number(0, 64))
    ite = Ite(cond, Num(Basic(62), Basic(64)), Num(Basic(63), Basic(64)))
    for i in range(62)[::-1]:
        cond = Neq(And(expr, Number(1 << i, 64)), Number(0, 64))
        ite = Ite(cond, Number(i, 64), ite)
    return ite

################ Helpers for Binary Operations ################

def opSIMD64(func, size, expr1, expr2):
    expr1 = sel64(size, expr1)
    expr2 = sel64(size, expr2)
    expr = map(lambda (x,y): func(x, y), zip(expr1, expr2))
    return mk64(size, expr)

def opSIMD128(func, size, expr1, expr2):
    expr1 = sel128(size, expr1)
    expr2 = sel128(size, expr2)
    expr = map(lambda (x,y): func(x, y), zip(expr1, expr2))
    return mk128(size, expr)

def Add8x8(expr1, expr2):
    return opSIMD64(Add, 8, expr1, expr2)

def Add16x4(expr1, expr2):
    return opSIMD64(Add, 16, expr1, expr2)

def Add32x2(expr1, expr2):
    return opSIMD64(Add, 32, expr1, expr2)

def Add8x16(expr1, expr2):
    return opSIMD128(Add, 8, expr1, expr2)

def Add16x8(expr1, expr2):
    return opSIMD128(Add, 16, expr1, expr2)

def Add32x4(expr1, expr2):
    return opSIMD128(Add, 32, expr1, expr2)

def Add64x2(expr1, expr2):
    return opSIMD128(Add, 64, expr1, expr2)

def QAdd8Sx8(expr1, expr2):
    return opSIMD64(qadd8S, 8, expr1, expr2)

def QAdd8Ux8(expr1, expr2):
    return opSIMD64(qadd8U, 8, expr1, expr2)

def QAdd16Sx4(expr1, expr2):
    return opSIMD64(qadd16S, 16, expr1, expr2)

def QAdd16Ux4(expr1, expr2):
    return opSIMD64(qadd16U, 16, expr1, expr2)

def QAdd8Sx16(expr1, expr2):
    return opSIMD128(qadd16S, 16, expr1, expr2)

def QAdd8Ux16(expr1, expr2):
    return opSIMD128(qadd16U, 16, expr1, expr2)

def QAdd16Sx8(expr1, expr2):
    return opSIMD128(qadd16S, 16, expr1, expr2)

def QAdd16Ux8(expr1, expr2):
    return opSIMD128(qadd16U, 16, expr1, expr2)

def Sub8x8(expr1, expr2):
    return opSIMD64(Sub, 8, expr1, expr2)

def Sub16x4(expr1, expr2):
    return opSIMD64(Sub, 16, expr1, expr2)

def Sub32x2(expr1, expr2):
    return opSIMD64(Sub, 32, expr1, expr2)

def Sub8x16(expr1, expr2):
    return opSIMD128(Sub, 8, expr1, expr2)

def Sub16x8(expr1, expr2):
    return opSIMD128(Sub, 16, expr1, expr2)

def Sub32x4(expr1, expr2):
    return opSIMD128(Sub, 32, expr1, expr2)

def Sub64x2(expr1, expr2):
    return opSIMD128(Sub, 64, expr1, expr2)

def QSub8Sx8(expr1, expr2):
    return opSIMD64(qsub8S, 8, expr1, expr2)

def QSub8Ux8(expr1, expr2):
    return opSIMD64(qsub8U, 8, expr1, expr2)

def QSub16Sx4(expr1, expr2):
    return opSIMD64(qsub16S, 16, expr1, expr2)

def QSub16Ux4(expr1, expr2):
    return opSIMD64(qsub16U, 16, expr1, expr2)

def QSub8Sx16(expr1, expr2):
    return opSIMD128(qsub8S, 8, expr1, expr2)

def QSub8Ux16(expr1, expr2):
    return opSIMD128(qsub8U, 8, expr1, expr2)

def QSub16Sx8(expr1, expr2):
    return opSIMD128(qsub16S, 16, expr1, expr2)

def QSub16Ux8(expr1, expr2):
    return opSIMD128(qsub16U, 16, expr1, expr2)

def MullU32(expr1, expr2):
    expr1 = Zero(64, expr1)
    expr2 = Zero(64, expr2)
    return Umul(expr1, expr2)

def Mul16x4(expr1, expr2):
    return opSIMD64(Smul, 16, expr1, expr2)

def Mul32x2(expr1, expr2):
    return opSIMD64(Smul, 32, expr1, expr2)

def Mul16x8(expr1, expr2):
    return opSIMD128(Smul, 16, expr1, expr2)

def MulHi16Sx4(expr1, expr2):
    return opSIMD64(mulhi16S, 16, expr1, expr2)

def MulHi16Ux4(expr1, expr2):
    return opSIMD64(mulhi16U, 16, expr1, expr2)

def MulHi16Sx8(expr1, expr2):
    return opSIMD128(mulhi16S, 16, expr1, expr2)

def MulHi16Ux8(expr1, expr2):
    return opSIMD128(mulhi16U, 16, expr1, expr2)

def ShlN8x8(expr1, expr2):
    expr1 = sel64(8, expr1)
    expr2 = [expr2 for i in xrange(8)]
    expr = map(lambda (x, y): Shl(x, y), zip(expr1, expr2))
    return mk64(8, expr)

def ShlN16x4(expr1, expr2):
    expr1 = sel64(16, expr1)
    expr2 = [expr2 for i in xrange(4)]
    expr = map(lambda (x, y): Shl(x, y), zip(expr1, expr2))
    return mk64(16, expr)

def ShlN32x2(expr1, expr2):
    expr1 = sel64(32, expr1)
    expr2 = [expr2 for i in xrange(2)]
    expr = map(lambda (x, y): Shl(x, y), zip(expr1, expr2))
    return mk64(32, expr)

def ShlN16x8(expr1, expr2):
    expr1 = sel128(16, expr1)
    expr2 = [expr2 for i in xrange(8)]
    expr = map(lambda (x, y): Shl(x, y), zip(expr1, expr2))
    return mk128(16, expr)

def ShlN32x4(expr1, expr2):
    expr1 = sel128(32, expr1)
    expr2 = [expr2 for i in xrange(4)]
    expr = map(lambda (x, y): Shl(x, y), zip(expr1, expr2))
    return mk128(32, expr)

def ShlN64x2(expr1, expr2):
    expr1 = sel128(64, expr1)
    expr2 = [expr2 for i in xrange(2)]
    expr = map(lambda (x, y): Shl(x, y), zip(expr1, expr2))
    return mk128(64, expr)

def ShrN16x4(expr1, expr2):
    expr1 = sel64(16, expr1)
    expr2 = [expr2 for i in xrange(4)]
    expr = map(lambda (x, y): Ushr(x, y), zip(expr1, expr2))
    return mk64(16, expr)

def ShrN32x2(expr1, expr2):
    expr1 = sel64(32, expr1)
    expr2 = [expr2 for i in xrange(2)]
    expr = map(lambda (x, y): Ushr(x, y), zip(expr1, expr2))
    return mk64(32, expr)

def ShrN16x8(expr1, expr2):
    expr1 = sel128(16, expr1)
    expr2 = [expr2 for i in xrange(8)]
    expr = map(lambda (x, y): Ushr(x, y), zip(expr1, expr2))
    return mk128(16, expr)

def ShrN32x4(expr1, expr2):
    expr1 = sel128(32, expr1)
    expr2 = [expr2 for i in xrange(4)]
    expr = map(lambda (x, y): Ushr(x, y), zip(expr1, expr2))
    return mk128(32, expr)

def ShrN64x2(expr1, expr2):
    expr1 = sel128(64, expr1)
    expr2 = [expr2 for i in xrange(2)]
    expr = map(lambda (x, y): Ushr(x, y), zip(expr1, expr2))
    return mk128(64, expr)

def SarN8x8(expr1, expr2):
    expr1 = sel64(8, expr1)
    expr2 = [expr2 for i in xrange(8)]
    expr = map(lambda (x, y): Sshr(x, y), zip(expr1, expr2))
    return mk64(8, expr)

def SarN16x4(expr1, expr2):
    expr1 = sel64(16, expr1)
    expr2 = [expr2 for i in xrange(4)]
    expr = map(lambda (x, y): Sshr(x, y), zip(expr1, expr2))
    return mk64(16, expr)

def SarN32x2(expr1, expr2):
    expr1 = sel64(32, expr1)
    expr2 = [expr2 for i in xrange(2)]
    expr = map(lambda (x, y): Sshr(x, y), zip(expr1, expr2))
    return mk64(32, expr)

def SarN16x8(expr1, expr2):
    expr1 = sel128(16, expr1)
    expr2 = [expr2 for i in xrange(8)]
    expr = map(lambda (x, y): Sshr(x, y), zip(expr1, expr2))
    return mk128(16, expr)

def SarN32x4(expr1, expr2):
    expr1 = sel128(32, expr1)
    expr2 = [expr2 for i in xrange(4)]
    expr = map(lambda (x, y): Sshr(x, y), zip(expr1, expr2))
    return mk128(32, expr)

def CmpEQ8x8(expr1, expr2):
    return opSIMD64(cmpeq8, 8, expr1, expr2)

def CmpEQ16x4(expr1, expr2):
    return opSIMD64(cmpeq16, 16, expr1, expr2)

def CmpEQ32x2(expr1, expr2):
    return opSIMD64(cmpeq32, 32, expr1, expr2)

def CmpEQ8x16(expr1, expr2):
    return opSIMD128(cmpeq8, 8, expr1, expr2)

def CmpEQ16x8(expr1, expr2):
    return opSIMD128(cmpeq16, 16, expr1, expr2)

def CmpEQ32x4(expr1, expr2):
    return opSIMD128(cmpeq32, 32, expr1, expr2)

def CmpGT8Sx8(expr1, expr2):
    return opSIMD64(cmpgt8S, 8, expr1, expr2)

def CmpGT16Sx4(expr1, expr2):
    return opSIMD64(cmpgt16S, 16, expr1, expr2)

def CmpGT32Sx2(expr1, expr2):
    return opSIMD64(cmpgt32S, 32, expr1, expr2)

def CmpGT8Sx16(expr1, expr2):
    return opSIMD128(cmpgt8S, 8, expr1, expr2)

def CmpGT16Sx8(expr1, expr2):
    return opSIMD128(cmpgt16S, 16, expr1, expr2)

def CmpGT32Sx4(expr1, expr2):
    return opSIMD128(cmpgt32S, 32, expr1, expr2)

def Perm8x8(expr1, expr2):
    expr1 = [expr1 for i in xrange(8)]
    expr2 = sel64(8, expr2)
    expr = map(lambda (x,y): index8x8(x, y), zip(expr1, expr2))
    return mk64(8, expr)

def CatOddLanes16x4(expr1, expr2):
    expr1 = sel64(16, expr1)
    expr2 = sel64(16, expr2)
    return mk16x4(expr1[3], expr1[1], expr2[3], expr2[1])

def CatEvenLanes16x4(expr1, expr2):
    expr1 = sel64(16, expr1)
    expr2 = sel64(16, expr2)
    return mk16x4(expr1[2], expr1[0], expr2[2], expr2[0])

def InterleaveHI8x8(expr1, expr2):
    expr1 = sel64(8, expr1)
    expr2 = sel64(8, expr2)
    return mk8x8(expr1[7], expr2[7], expr1[6], expr2[6],
                 expr1[5], expr2[5], expr1[4], expr2[4])

def InterleaveHI16x4(expr1, expr2):
    expr1 = sel64(16, expr1)
    expr2 = sel64(16, expr2)
    return mk16x4(expr1[3], expr2[3], expr1[2], expr2[2])

def InterleaveHI32x2(expr1, expr2):
    expr1 = sel64(32, expr1)
    expr2 = sel64(32, expr2)
    return mk32x2(expr1[1], expr2[1])

def InterleaveHI8x16(expr1, expr2):
    expr1 = sel128(8, expr1)
    expr2 = sel128(8, expr2)
    return mk8x16(expr1[15], expr2[15], expr1[14], expr2[14],
                  expr1[13], expr2[13], expr1[12], expr2[12],
                  expr1[11], expr2[11], expr1[10], expr2[10],
                  expr1[9], expr2[9], expr1[8], expr2[8])

def InterleaveHI16x8(expr1, expr2):
    expr1 = sel128(16, expr1)
    expr2 = sel128(16, expr2)
    return mk16x8(expr1[7], expr2[7], expr1[6], expr2[6],
                  expr1[5], expr2[5], expr1[4], expr2[4])

def InterleaveHI32x4(expr1, expr2):
    expr1 = sel128(32, expr1)
    expr2 = sel128(32, expr2)
    return mk32x4(expr1[3], expr2[3], expr1[2], expr2[2])

def InterleaveLO8x8(expr1, expr2):
    expr1 = sel64(8, expr1)
    expr2 = sel64(8, expr2)
    return mk8x8(expr1[3], expr2[3], expr1[2], expr2[2],
                 expr1[1], expr2[1], expr1[0], expr2[0])

def InterleaveLO16x4(expr1, expr2):
    expr1 = sel64(16, expr1)
    expr2 = sel64(16, expr2)
    return mk16x4(expr1[1], expr2[1], expr1[0], expr2[0])

def InterleaveLO32x2(expr1, expr2):
    expr1 = sel64(32, expr1)
    expr2 = sel64(32, expr2)
    return mk32x2(expr1[0], expr2[0])

def InterleaveLO8x16(expr1, expr2):
    expr1 = sel128(8, expr1)
    expr2 = sel128(8, expr2)
    return mk8x16(expr1[7], expr2[7], expr1[6], expr2[6],
                  expr1[5], expr2[5], expr1[4], expr2[4],
                  expr1[3], expr2[3], expr1[2], expr2[2],
                  expr1[1], expr2[1], expr1[0], expr2[0])

def InterleaveLO16x8(expr1, expr2):
    expr1 = sel128(16, expr1)
    expr2 = sel128(16, expr2)
    return mk16x8(expr1[3], expr2[3], expr1[2], expr2[2],
                  expr1[1], expr2[1], expr1[0], expr2[0])

def InterleaveLO32x4(expr1, expr2):
    expr1 = sel128(32, expr1)
    expr2 = sel128(32, expr2)
    return mk32x4(expr1[1], expr2[1], expr1[0], expr2[0])

def Max8Ux8(expr1, expr2):
    return opSIMD64(max8U, 8, expr1, expr2)

def Max16Sx4(expr1, expr2):
    return opSIMD64(max16S, 16, expr1, expr2)

def Max8Ux16(expr1, expr2):
    return opSIMD128(max8U, 8, expr1, expr2)

def Max16Sx8(expr1, expr2):
    return opSIMD128(max16S, 16, expr1, expr2)

def Min8Ux8(expr1, expr2):
    return opSIMD64(min8U, 8, expr1, expr2)

def Min16Sx4(expr1, expr2):
    return opSIMD64(min16S, 16, expr1, expr2)

def Min8Ux16(expr1, expr2):
    return opSIMD128(min8U, 8, expr1, expr2)

def Min16Sx8(expr1, expr2):
    return opSIMD128(min16S, 16, expr1, expr2)

def Avg8Ux8(expr1, expr2):
    return opSIMD64(avg8U, 8, expr1, expr2)

def Avg16Ux4(expr1, expr2):
    return opSIMD64(avg16U, 16, expr1, expr2)

def Avg8Ux16(expr1, expr2):
    return opSIMD128(avg8U, 16, expr1, expr2)

def Avg16Ux8(expr1, expr2):
    return opSIMD128(avg16U, 16, expr1, expr2)

def QNarrowBin16Sto8Sx8(expr1, expr2):
    expr1 = sel64(16, expr1)
    expr2 = sel64(16, expr2)
    expr1 = map(qnarrow16Sto8S, expr1)
    expr2 = map(qnarrow16Sto8S, expr2)
    return mk8x8(expr1[3], expr1[2], expr1[1], expr1[0],
                 expr2[3], expr2[2], expr2[1], expr2[0])

def QNarrowBin16Sto8Sx16(expr1, expr2):
    expr1 = sel128(16, expr1)
    expr2 = sel128(16, expr2)
    expr1 = map(qnarrow16Sto8S, expr1)
    expr2 = map(qnarrow16Sto8S, expr2)
    return mk8x16(expr1[7], expr1[6], expr1[5], expr1[4],
                  expr1[3], expr1[2], expr1[1], expr1[0],
                  expr2[7], expr2[6], expr2[5], expr2[4],
                  expr2[3], expr2[2], expr2[1], expr2[0])

def QNarrowBin16Sto8Ux8(expr1, expr2):
    expr1 = sel64(16, expr1)
    expr2 = sel64(16, expr2)
    expr1 = map(qnarrow16Sto8U, expr1)
    expr2 = map(qnarrow16Sto8U, expr2)
    return mk8x8(expr1[3], expr1[2], expr1[1], expr1[0],
                 expr2[3], expr2[2], expr2[1], expr2[0])

def QNarrowBin16Sto8Ux16(expr1, expr2):
    expr1 = sel128(16, expr1)
    expr2 = sel128(16, expr2)
    expr1 = map(qnarrow16Sto8U, expr1)
    expr2 = map(qnarrow16Sto8U, expr2)
    return mk8x16(expr1[7], expr1[6], expr1[5], expr1[4],
                  expr1[3], expr1[2], expr1[1], expr1[0],
                  expr2[7], expr2[6], expr2[5], expr2[4],
                  expr2[3], expr2[2], expr2[1], expr2[0])

def QNarrowBin32Sto16Sx4(expr1, expr2):
    expr1 = sel64(32, expr1)
    expr2 = sel64(32, expr2)
    expr1 = map(qnarrow32Sto16S, expr1)
    expr2 = map(qnarrow32Sto16S, expr2)
    return mk16x4(expr1[1], expr1[0], expr2[1], expr2[0])

def QNarrowBin32Sto16Sx8(expr1, expr2):
    expr1 = sel128(32, expr1)
    expr2 = sel128(32, expr2)
    expr1 = map(qnarrow32Sto16S, expr1)
    expr2 = map(qnarrow32Sto16S, expr2)
    return mk16x8(expr1[3], expr1[2], expr1[1], expr1[0],
                  expr2[3], expr2[2], expr2[1], expr2[0])

def SetV128lo32(expr1, expr2):
    expr1 = Shl_n(32, 128, Ushr_n(32, 128, expr1))
    expr2 = Zero(128, expr2)
    return Or(expr1, expr2)

def SetV128lo64(expr1, expr2):
    expr1 = Shl_n(64, 128, Ushr_n(64, 128, expr1))
    expr2 = Zero(128, expr2)
    return Or(expr1, expr2)

unop_map ={
    'Not8' : Not, 'Not64' : Not, 'NotV128' : Not,
    '32to1' : (lambda x: Low(1, x)), '32to8' : (lambda x: Low(8, x)),
    '32to16' : (lambda x: Low(16, x)), '64to32' : (lambda x: Low(32, x)),
    '64to8' : (lambda x: Low(8, x)), '64to1' : (lambda x: Low(1, x)),
    'V128to64' : (lambda x: Low(64, x)),
    '32HIto16' : (lambda x: High(16, x)), '64HIto32' : (lambda x: High(32, x)),
    'V128HIto64' : (lambda x: High(64, x)),
    '1Uto8' : (lambda x: Zero(8, x)), '1Uto32' : (lambda x: Zero(32, x)),
    '1Uto64' : (lambda x: Zero(64, x)),
    '8Uto16' : (lambda x: Zero(16, x)),
    '8Uto32' : (lambda x: Zero(32, x)), '8Uto64' : (lambda x: Zero(64, x)),
    '16Uto32' : (lambda x: Zero(32, x)), '16Uto64' : (lambda x: Zero(64, x)),
    '32Uto64' : (lambda x: Zero(64, x)), '32UtoV128' : (lambda x: Zero(128, x)),
    '64UtoV128' : (lambda x: Zero(128, x)),
    '8Sto16' : (lambda x: Sign(16, x)),
    '8Sto32' : (lambda x: Sign(32, x)), '16Sto32' : (lambda x: Sign(32, x)),
    '32Sto64' : (lambda x: Sign(64, x)),
    'Clz32' : Clz32, 'Clz64' : Clz64, 'Ctz32' : Ctz32, 'Ctz64' : Ctz64,
}

def fetch_unop(op, expr):
    op = op.split('Iop_')[1]
    if 'F' in op:
        raise IncapableError
    elif op in unop_map.keys():
        return unop_map[op](expr)
    else:
        print op
        raise NotImplemented

binop_map = {
    'Add8' : Add, 'Add16' : Add, 'Add32' : Add, 'Add64' : Add,
    'Add8x8' : Add8x8, 'Add16x4' : Add16x4, 'Add32x2' : Add32x2,
    'Add8x16' : Add8x16, 'Add16x8' : Add16x8, 'Add32x4' : Add32x4, 'Add64x2' : Add64x2,
    'QAdd8Sx8' : QAdd8Sx8, 'QAdd8Ux8' : QAdd8Ux8,
    'QAdd16Sx4' : QAdd16Sx4, 'QAdd16Ux4' : QAdd16Ux4,
    'QAdd8Sx16' : QAdd8Sx16, 'QAdd8Ux16' : QAdd8Ux16,
    'QAdd16Sx8' : QAdd16Sx8, 'QAdd16Ux8' : QAdd16Ux8,
    'Sub8' : Sub, 'Sub16' : Sub, 'Sub32' : Sub, 'Sub64' : Sub,
    'Sub8x8' : Sub8x8, 'Sub16x4' : Sub16x4, 'Sub32x2' : Sub32x2,
    'Sub8x16' : Sub8x16, 'Sub16x8' : Sub16x8, 'Sub32x4' : Sub32x4, 'Sub64x2' : Sub64x2,
    'QSub8Sx8' : QSub8Sx8, 'QSub8Ux8' : QSub8Ux8,
    'QSub16Sx4' : QSub16Sx4, 'QSub16Ux4' : QSub16Ux4,
    'QSub8Sx16' : QSub8Sx16, 'QSub8Ux16' : QSub8Ux16,
    'QSub16Sx8' : QSub16Sx8, 'QSub16Ux8' : QSub16Ux8,
    'Mul16' : Smul, 'Mul32' : Smul, 'MullU32' : MullU32,
    'MulHi16Sx4' : MulHi16Sx4, 'MulHi16Ux4' : MulHi16Ux4,
    'MulHi16Sx8' : MulHi16Sx8, 'MulHi16Ux8' : MulHi16Ux8,
    'Mul16x4' : Mul16x4, 'Mul32x2' : Mul32x2,
    'Mul16x8' : Mul16x8,
    'Shl8' : Shl, 'Shl16' : Shl, 'Shl32' : Shl, 'Shl64' : Shl,
    'ShlN8x8' : ShlN8x8, 'ShlN16x4' : ShlN16x4, 'ShlN32x2' : ShlN32x2,
    'ShlN16x8' : ShlN16x8, 'ShlN32x4' : ShlN32x4, 'ShlN64x2' : ShlN64x2,
    'Shr8' : Ushr, 'Shr16' : Ushr, 'Shr32' : Ushr, 'Shr64' : Ushr,
    'ShrN16x4' : ShrN16x4, 'ShrN32x2' : ShrN32x2,
    'ShrN16x8' : ShrN16x8, 'ShrN32x4' : ShrN32x4, 'ShrN64x2' : ShrN64x2,
    'Sar16' : Sshr, 'Sar32' : Sshr, 'Sar64' : Sshr,
    'SarN8x8' : SarN8x8, 'SarN16x4' : SarN16x4, 'SarN32x2' : SarN32x2,
    'SarN16x8' : SarN16x8, 'SarN32x4' : SarN32x4,
    'And8' : And, 'And16' : And, 'And32' : And, 'And64' : And, 'AndV128' : And,
    'Or8' : Or, 'Or16' : Or, 'Or32' : Or, 'Or64' : Or, 'OrV128' : Or,
    'Xor8' : Xor, 'Xor16' : Xor, 'Xor32' : Xor, 'Xor64' : Xor, 'XorV128' : Xor,
    '16HLto32' : Concat, '32HLto64' : Concat, '64HLtoV128' : Concat,
    'CmpEQ8' : Eq, 'CmpEQ16' : Eq, 'CmpEQ32' : Eq, 'CmpEQ64' : Eq,
    'CmpEQ8x8' : CmpEQ8x8, 'CmpEQ16x4' : CmpEQ16x4, 'CmpEQ32x2' : CmpEQ32x2,
    'CmpEQ8x16' : CmpEQ8x16, 'CmpEQ16x8' : CmpEQ16x8, 'CmpEQ32x4' : CmpEQ32x4,
    'CmpNE8' : Neq, 'CmpNE16' : Neq, 'CmpNE32' : Neq, 'CmpNE64' : Neq,
    'CasCmpNE8' : Neq, 'CasCmpNE16' : Neq, 'CasCmpNE32' : Neq,
    'ExpCmpNE16' : Neq, 'ExpCmpNE32' : Neq, 'ExpCmpNE64' : Neq,
    'CmpLT32U' : Ult, 'CmpLT64U' : Ult, 'CmpGT8Sx8' : CmpGT8Sx8,
    'CmpGT16Sx4' : CmpGT16Sx4, 'CmpGT32Sx2' : CmpGT32Sx2,
    'CmpGT8Sx16' : CmpGT8Sx16, 'CmpGT16Sx8' : CmpGT16Sx8, 'CmpGT32Sx4' : CmpGT32Sx4,
    'Max8Ux8' : Max8Ux8, 'Max16Sx4' : Max16Sx4,
    'Max8Ux16' : Max8Ux16, 'Max16Sx8' : Max16Sx8,
    'Min8Ux8' : Min8Ux8, 'Min16Sx4' : Min16Sx4,
    'Min8Ux16' : Min8Ux16, 'Min16Sx8' : Min16Sx8,
    'Avg8Ux8' : Avg8Ux8, 'Avg16Ux4' : Avg16Ux4,
    'Avg8Ux16' : Avg8Ux16, 'Avg16Ux8' : Avg16Ux8,
    'Perm8x8' : Perm8x8,
    'CatOddLanes16x4' : CatOddLanes16x4, 'CatEvenLanes16x4' : CatEvenLanes16x4,
    'InterleaveLO8x8' : InterleaveLO8x8, 'InterleaveLO16x4' : InterleaveLO16x4,
    'InterleaveLO32x2' : InterleaveLO32x2, 'InterleaveHI8x8' : InterleaveHI8x8,
    'InterleaveLO8x16' : InterleaveLO8x16,
    'InterleaveLO16x8' : InterleaveLO16x8, 'InterleaveLO32x4' : InterleaveLO32x4,
    'InterleaveHI16x4' : InterleaveHI16x4, 'InterleaveHI32x2' : InterleaveHI32x2,
    'InterleaveHI8x16' : InterleaveHI8x16,
    'InterleaveHI16x8' : InterleaveHI16x8, 'InterleaveHI32x4' : InterleaveHI32x4,
    'QNarrowBin16Sto8Sx8' : QNarrowBin16Sto8Sx8,
    'QNarrowBin16Sto8Sx16' : QNarrowBin16Sto8Sx16,
    'QNarrowBin16Sto8Ux8' : QNarrowBin16Sto8Ux8,
    'QNarrowBin16Sto8Ux16' : QNarrowBin16Sto8Ux16,
    'QNarrowBin32Sto16Sx4' : QNarrowBin32Sto16Sx4,
    'QNarrowBin32Sto16Sx8' : QNarrowBin32Sto16Sx8,
    'SetV128lo32' : SetV128lo32, 'SetV128lo64' : SetV128lo64,
}

def fetch_binop(op, expr1, expr2):
    op = op.split('Iop_')[1]
    if 'F' in op:
        raise IncapableError
    elif op in binop_map.keys():
        return binop_map[op](expr1, expr2)
    else:
        print op
        raise NotImplemented

CF_POS = 0
PF_POS = 2
AF_POS = 4
ZF_POS = 6
SF_POS = 7
OF_POS = 11

CF_MASK = 1 << CF_POS
PF_MASK = 1 << PF_POS
AF_MASK = 1 << AF_POS
ZF_MASK = 1 << ZF_POS
SF_MASK = 1 << SF_POS
OF_MASK = 1 << OF_POS

def eflags32_lshift(n, expr):
    if n > 0:
        return Shl_n(n, 32, expr)
    elif n == 0:
        return expr
    else:
        return Ushr_n(-n, 32, expr)

def eflags32_cf1(arg0, arg1, size):
    return Low(1, Ult(arg0, arg1))

def eflags32_cf2(arg0, arg1, arg2, size):
    t0 = Neq(arg0, Number(0, 32))
    t1 = Ule(arg1, arg2)
    t2 = Ult(arg1, arg2)
    return Low(1, Ite(t0, t1, t2))

def eflags32_cf3(arg0, size, flag):
    if flag == 1:
        t0 = Ushr(arg0, Number(size - 1, 32))
    else:
        t0 = arg0
    return Low(1, t0)

def eflags32_cf4(arg0, arg1, size, flag):
    if flag == 1:
        return Neq(arg0, Number(0, size))
    else:
        return Neq(arg0, Sshr(arg1, Number(size - 1, size)))

def eflags32_pf(arg0):
    t0 = Xor(Ushr(arg0, Number(4, 8)), arg0)
    t1 = Xor(Ushr(t0, Number(2, 8)), t0)
    t2 = Xor(Ushr(t1, Number(1, 8)), t1)
    t3 = Low(1, t2)
    return Not(t3)

def eflags32_af(arg0, arg1, arg2):
    t0 = Xor(Xor(arg0, arg1), arg2)
    t1 = And(t0, Number(AF_MASK, 32))
    t2 = Ushr(t1, Number(AF_POS, 32))
    return Low(1, t2)

def eflags32_zf(arg, size):
    t0 = Eq(arg, Number(0, size))
    return Low(1, t0)

def eflags32_sf(arg, size):
    t0 = eflags32_lshift(8 - size, arg)
    t1 = And(t0, Number(SF_MASK, 32))
    t2 = Ushr(t1, Number(SF_POS, 32))
    return Low(1, t2)

def eflags32_of1(arg0, arg1, arg2, size, flag):
    t0 = Xor(arg0, arg1)
    if flag == 1:
        t1 = Xor(t0, Number(0xffffffff, 32))
    else:
        t1 = t0
    t2 = Xor(arg0, arg2)
    t3 = And(t1, t2)
    t4 = eflags32_lshift(12 - size, t3)
    t5 = And(t4, Number(OF_MASK, 32))
    t6 = Ushr(t5, Number(OF_POS, 32))
    return Low(1, t6)

def eflags32_of2(arg, size, flag):
    if size == 8:
        data_mask = Number(0xff, 32)
    elif size == 16:
        data_mask = Number(0xffff, 32)
    else:
        data_mask = Number(0xffffffff, 32)
    sign_mask = Number(1 << (size - 1), 32)
    t0 = And(arg, data_mask)
    if flag == 1:
        t1 = sign_mask
    else:
        t1 = Sub(sign_mask, Number(1, 32))
    t2 = Eq(t0, t1)
    return Low(1, t2)

def eflags32_of3(arg0, arg1, size):
    t0 = eflags32_lshift(12 - size, Xor(arg0, arg1))
    t1 = And(t0, Number(OF_MASK, 32))
    t2 = Ushr(t1, Number(OF_POS, 32))
    return Low(1, t2)

def eflags32_of4(arg0):
    return arg0

def eflags32_fl_to_bit(fl, mask, pos):
    return Low(1, Ushr_n(pos, 32, And(fl, Number(mask, 32))))

def eflags32_copy(cc_dep1):
    flags = {}
    flags['cf'] = eflags32_fl_to_bit(cc_dep1, CF_MASK, CF_POS)
    flags['pf'] = eflags32_fl_to_bit(cc_dep1, PF_MASK, PF_POS)
    flags['af'] = eflags32_fl_to_bit(cc_dep1, AF_MASK, AF_POS)
    flags['zf'] = eflags32_fl_to_bit(cc_dep1, ZF_MASK, ZF_POS)
    flags['sf'] = eflags32_fl_to_bit(cc_dep1, SF_MASK, SF_POS)
    flags['of'] = eflags32_fl_to_bit(cc_dep1, OF_MASK, OF_POS)

    return flags

def eflags32_add(size, cc_dep1, cc_dep2):
    argL = cc_dep1
    argR = cc_dep2
    res = Add(argL, argR)
    res_t = Low(size, res)
    argL_t = Low(size, argL)
    res_c = Low(8, res)

    flags = {}
    flags['cf'] = eflags32_cf1(res_t, argL_t, size)
    flags['pf'] = eflags32_pf(res_c)
    flags['af'] = eflags32_af(res, argL, argR)
    flags['zf'] = eflags32_zf(res_t, size)
    flags['sf'] = eflags32_sf(res, size)
    flags['of'] = eflags32_of1(argL, argR, res, size, 1)

    return flags

def eflags32_sub(size, cc_dep1, cc_dep2):
    argL = cc_dep1
    argR = cc_dep2
    res = Sub(argL, argR)
    res_t = Low(size, res)
    argL_t = Low(size, argL)
    argR_t = Low(size, argR)
    res_c = Low(8, res)

    flags = {}
    flags['cf'] = eflags32_cf1(argL_t, argR_t, size)
    flags['pf'] = eflags32_pf(res_c)
    flags['af'] = eflags32_af(res, argL, argR)
    flags['zf'] = eflags32_zf(res_t, size)
    flags['sf'] = eflags32_sf(res, size)
    flags['of'] = eflags32_of1(argL, argR, res, size, 0)

    return flags

def eflags32_adc(size, cc_dep1, cc_dep2, cc_ndep):
    oldC = And(cc_ndep, Number(CF_MASK, 32))
    argL = cc_dep1
    argR = Xor(cc_dep2, oldC)
    res = Add(Add(argL, argR), oldC)
    res_t = Low(size, res)
    argL_t = Low(size, argL)
    res_c = Low(8, res)

    flags = {}
    flags['cf'] = eflags32_cf2(oldC, res_t, argL_t, size)
    flags['pf'] = eflags32_pf(res_c)
    flags['af'] = eflags32_af(res, argL, argR)
    flags['zf'] = eflags32_zf(res_t, size)
    flags['sf'] = eflags32_sf(res, size)
    flags['of'] = eflags32_of1(argL, argR, res, size, 1)

    return flags

def eflags32_sbb(size, cc_dep1, cc_dep2, cc_ndep):
    oldC = And(cc_ndep, Number(CF_MASK, 32))
    argL = cc_dep1
    argR = Xor(cc_dep2, oldC)
    res = Sub(Sub(argL, argR), oldC)
    res_t = Low(size, res)
    argL_t = Low(size, argL)
    argR_t = Low(size, argR)
    res_c = Low(8, res)

    flags = {}
    flags['cf'] = eflags32_cf2(oldC, argL_t, argR_t, size)
    flags['pf'] = eflags32_pf(res_c)
    flags['af'] = eflags32_af(res, argL, argR)
    flags['zf'] = eflags32_zf(res_t, size)
    flags['sf'] = eflags32_sf(res, size)
    flags['of'] = eflags32_of1(argL, argR, res, size, 0)

    return flags

def eflags32_logic(size, cc_dep1):
    cc_dep1_t = Low(size, cc_dep1)
    cc_dep1_c = Low(9, cc_dep1)

    flags = {}
    flags['cf'] = Number(0, 1)
    flags['pf'] = eflags32_pf(cc_dep1_c)
    flags['af'] = Number(0, 1)
    flags['zf'] = eflags32_zf(cc_dep1_t, size)
    flags['sf'] = eflags32_sf(cc_dep1, size)
    flags['of'] = Number(0, 1)

    return flags

def eflags32_inc(size, cc_dep1):
    res = cc_dep1
    argL = Sub(res, Number(1, 32))
    argR = Number(1, 32)
    res_t = Low(size, res)
    res_c = Low(8, res)

    flags = {}
    flags['cf'] = None
    flags['pf'] = eflags32_pf(res_c)
    flags['af'] = eflags32_af(res, argL, argR)
    flags['zf'] = eflags32_zf(res_t, size)
    flags['sf'] = eflags32_sf(res, size)
    flags['of'] = eflags32_of2(res, size, 1)

    return flags

def eflags32_dec(size, cc_dep1):
    res = cc_dep1
    argL = Add(res, Number(1, 32))
    argR = Number(1, 32)
    res_t = Low(size, res)
    res_c = Low(8, res)

    flags = {}
    flags['cf'] = None
    flags['pf'] = eflags32_pf(res_c)
    flags['af'] = eflags32_af(res, argL, argR)
    flags['zf'] = eflags32_zf(res_t, size)
    flags['sf'] = eflags32_sf(res, size)
    flags['of'] = eflags32_of2(res, size, 0)

    return flags

def eflags32_shl(size, cc_dep1, cc_dep2):
    cc_dep1_t = Low(size, cc_dep1)
    cc_dep1_c = Low(8, cc_dep1)

    flags = {}
    flags['cf'] = eflags32_cf3(cc_dep2, size, 1)
    flags['pf'] = eflags32_pf(cc_dep1_c)
    flags['af'] = Number(0, 1)
    flags['zf'] = eflags32_zf(cc_dep1_t, size)
    flags['sf'] = eflags32_sf(cc_dep1, size)
    flags['of'] = eflags32_of3(cc_dep2, cc_dep1, size)

    return flags

def eflags32_shr(size, cc_dep1, cc_dep2):
    cc_dep1_t = Low(size, cc_dep1)
    cc_dep1_c = Low(8, cc_dep1)

    flags = {}
    flags['cf'] = eflags32_cf3(cc_dep2, size, 0)
    flags['pf'] = eflags32_pf(cc_dep1_c)
    flags['af'] = Number(0, 1)
    flags['zf'] = eflags32_zf(cc_dep1_t, size)
    flags['sf'] = eflags32_sf(cc_dep1, size)
    flags['of'] = eflags32_of3(cc_dep2, cc_dep1, size)

    return flags

def eflags32_rol(size, cc_dep1, cc_ndep):
    fl = And(cc_ndep, Not(Or(Number(OF_MASK, 32), Number(CF_MASK, 32))))
    fl = Or(fl, And(Number(CF_MASK, 32), cc_dep1))
    fl = Or(fl, And(Number(OF_MASK, 32),
                    Xor(eflags32_lshift(12 - size, cc_dep1),
                        eflags32_lshift(11, cc_dep1))))

    flags = {}
    flags['cf'] = eflags32_fl_to_bit(fl, CF_MASK, CF_POS)
    flags['pf'] = eflags32_fl_to_bit(fl, PF_MASK, PF_POS)
    flags['af'] = eflags32_fl_to_bit(fl, AF_MASK, AF_POS)
    flags['zf'] = eflags32_fl_to_bit(fl, ZF_MASK, ZF_POS)
    flags['sf'] = eflags32_fl_to_bit(fl, SF_MASK, SF_POS)
    flags['of'] = eflags32_fl_to_bit(fl, OF_MASK, OF_POS)

    return flags

def eflags32_ror(size, cc_dep1, cc_ndep):
    fl = And(cc_ndep, Not(Or(Number(OF_MASK, 32), Number(CF_MASK, 32))))
    fl = Or(fl, And(Number(CF_MASK, 32), Ushr_n(size - 1, 32, cc_dep1)))
    fl = Or(fl, And(Number(OF_MASK, 32),
                    Xor(eflags32_lshift(12 - size, cc_dep1),
                        eflags32_lshift(11, cc_dep1))))

    flags = {}
    flags['cf'] = eflags32_fl_to_bit(fl, CF_MASK, CF_POS)
    flags['pf'] = eflags32_fl_to_bit(fl, PF_MASK, PF_POS)
    flags['af'] = eflags32_fl_to_bit(fl, AF_MASK, AF_POS)
    flags['zf'] = eflags32_fl_to_bit(fl, ZF_MASK, ZF_POS)
    flags['sf'] = eflags32_fl_to_bit(fl, SF_MASK, SF_POS)
    flags['of'] = eflags32_fl_to_bit(fl, OF_MASK, OF_POS)

    return flags

def eflags32_umul(size, cc_dep1, cc_dep2):
    cc_dep1_t = Low(32, cc_dep1)
    cc_dep2_t = Low(32, cc_dep2)
    cc_dep1_2t = Zero(2 * size, cc_dep1_t)
    cc_dep2_2t = Zero(2 * size, cc_dep2_t)
    lo = Umul(cc_dep1_t, cc_dep1_t)
    rr = Umul(cc_dep1_2t, cc_dep2_2t)
    hi = Low(size, Ushr_n(size, 2 * size, rr))

    flags = {}
    flags['cf'] = eflags32_cf4(hi, lo, size, 1)
    flags['pf'] = eflags32_pf(Low(8, lo))
    flags['af'] = Number(0, 1)
    flags['zf'] = eflags32_zf(lo, size)
    flags['sf'] = eflags32_sf(lo, size)
    flags['of'] = eflags32_of4(flags['cf'])

    return flags

def eflags32_smul(size, cc_dep1, cc_dep2):
    cc_dep1_t = Low(32, cc_dep1)
    cc_dep2_t = Low(32, cc_dep2)
    cc_dep1_2t = Sign(2 * size, cc_dep1_t)
    cc_dep2_2t = Sign(2 * size, cc_dep2_t)
    lo = Low(size, Smul(cc_dep1_2t, cc_dep1_2t))
    rr = Smul(cc_dep1_2t, cc_dep2_2t)
    hi = Low(size, Sshr_n(size, 2 * size, rr))

    flags = {}
    flags['cf'] = eflags32_cf4(hi, lo, size, 0)
    flags['pf'] = eflags32_pf(Low(8, lo))
    flags['af'] = Number(0, 1)
    flags['zf'] = eflags32_zf(lo, size)
    flags['sf'] = eflags32_sf(lo, size)
    flags['of'] = eflags32_of4(flags['cf'])

    return flags
