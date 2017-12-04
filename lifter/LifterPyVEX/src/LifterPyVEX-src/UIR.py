import JSON_converter

class Basic:
    def __init__(self, value):
        self.value = value

    def to_JSON(self):
        return str(self.value).rstrip('L')

class Base:
    def __init__(self):
        self.sty = ''
        self.args = []

class EndianT(Base):
    def __init__(self):
        Base.__init__(self)

    def to_JSON(self):
        return JSON_converter.to_JSON('EndianT', self.sty, self.args)

class BE(EndianT):
    def __init__(self):
        EndianT.__init__(self)
        self.sty = 'BE'

    def to_JSON(self):
        return EndianT.to_JSON(self)

class LE(EndianT):
    def __init__(self):
        EndianT.__init__(self)
        self.sty = 'LE'

    def to_JSON(self):
        return EndianT.to_JSON(self)

class UnOpT(Base):
    def __init__(self):
        Base.__init__(self)

    def to_JSON(self):
        return JSON_converter.to_JSON('UnOpT', self.sty, self.args)

class NEG(UnOpT):
    def __init__(self):
        UnOpT.__init__(self)
        self.sty = 'NEG'

    def to_JSON(self):
        return UnOpT.to_JSON(self)

class NOT(UnOpT):
    def __init__(self):
        UnOpT.__init__(self)
        self.sty = 'NOT'

    def to_JSON(self):
        return UnOpT.to_JSON(self)

class BinOpT(Base):
    def __init__(self):
        Base.__init__(self)

    def to_JSON(self):
        return JSON_converter.to_JSON('BinOpT', self.sty, self.args)

class ADD(BinOpT):
    def __init__(self):
        BinOpT.__init__(self)
        self.sty = 'ADD'

    def to_JSON(self):
        return BinOpT.to_JSON(self)

class SUB(BinOpT):
    def __init__(self):
        BinOpT.__init__(self)
        self.sty = 'SUB'

    def to_JSON(self):
        return BinOpT.to_JSON(self)

class UMUL(BinOpT):
    def __init__(self):
        BinOpT.__init__(self)
        self.sty = 'UMUL'

    def to_JSON(self):
        return BinOpT.to_JSON(self)

class SMUL(BinOpT):
    def __init__(self):
        BinOpT.__init__(self)
        self.sty = 'SMUL'

    def to_JSON(self):
        return BinOpT.to_JSON(self)

class UDIV(BinOpT):
    def __init__(self):
        BinOpT.__init__(self)
        self.sty = 'UDIV'

    def to_JSON(self):
        return BinOpT.to_JSON(self)

class SDIV(BinOpT):
    def __init__(self):
        BinOpT.__init__(self)
        self.sty = 'SDIV'

    def to_JSON(self):
        return BinOpT.to_JSON(self)

class UMOD(BinOpT):
    def __init__(self):
        BinOpT.__init__(self)
        self.sty = 'UMOD'

    def to_JSON(self):
        return BinOpT.to_JSON(self)

class SMOD(BinOpT):
    def __init__(self):
        BinOpT.__init__(self)
        self.sty = 'SMOD'

    def to_JSON(self):
        return BinOpT.to_JSON(self)

class SHL(BinOpT):
    def __init__(self):
        BinOpT.__init__(self)
        self.sty = 'SHL'

    def to_JSON(self):
        return BinOpT.to_JSON(self)

class USHR(BinOpT):
    def __init__(self):
        BinOpT.__init__(self)
        self.sty = 'USHR'

    def to_JSON(self):
        return BinOpT.to_JSON(self)

class SSHR(BinOpT):
    def __init__(self):
        BinOpT.__init__(self)
        self.sty = 'SSHR'

    def to_JSON(self):
        return BinOpT.to_JSON(self)

class AND(BinOpT):
    def __init__(self):
        BinOpT.__init__(self)
        self.sty = 'AND'

    def to_JSON(self):
        return BinOpT.to_JSON(self)

class OR(BinOpT):
    def __init__(self):
        BinOpT.__init__(self)
        self.sty = 'OR'

    def to_JSON(self):
        return BinOpT.to_JSON(self)

class XOR(BinOpT):
    def __init__(self):
        BinOpT.__init__(self)
        self.sty = 'XOR'

    def to_JSON(self):
        return BinOpT.to_JSON(self)

class CONCAT(BinOpT):
    def __init__(self):
        BinOpT.__init__(self)
        self.sty = 'CONCAT'

    def to_JSON(self):
        return BinOpT.to_JSON(self)

class RelOpT(Base):
    def __init__(self):
        Base.__init__(self)

    def to_JSON(self):
        return JSON_converter.to_JSON('RelOpT', self.sty, self.args)

class EQ(RelOpT):
    def __init__(self):
        RelOpT.__init__(self)
        self.sty = 'EQ'

    def to_JSON(self):
        return RelOpT.to_JSON(self)

class NEQ(RelOpT):
    def __init__(self):
        RelOpT.__init__(self)
        self.sty = 'NEQ'

    def to_JSON(self):
        return RelOpT.to_JSON(self)

class ULT(RelOpT):
    def __init__(self):
        RelOpT.__init__(self)
        self.sty = 'ULT'

    def to_JSON(self):
        return RelOpT.to_JSON(self)

class SLT(RelOpT):
    def __init__(self):
        RelOpT.__init__(self)
        self.sty = 'SLT'

    def to_JSON(self):
        return RelOpT.to_JSON(self)

class ULE(RelOpT):
    def __init__(self):
        RelOpT.__init__(self)
        self.sty = 'ULE'

    def to_JSON(self):
        return RelOpT.to_JSON(self)

class SLE(RelOpT):
    def __init__(self):
        RelOpT.__init__(self)
        self.sty = 'SLE'

    def to_JSON(self):
        return RelOpT.to_JSON(self)

class CastOpT(Base):
    def __init__(self):
        Base.__init__(self)

    def to_JSON(self):
        return JSON_converter.to_JSON('CastOpT', self.sty, self.args)

class LOW(CastOpT):
    def __init__(self):
        CastOpT.__init__(self)
        self.sty = 'LOW'

    def to_JSON(self):
        return CastOpT.to_JSON(self)

class HIGH(CastOpT):
    def __init__(self):
        CastOpT.__init__(self)
        self.sty = 'HIGH'

    def to_JSON(self):
        return CastOpT.to_JSON(self)

class ZERO(CastOpT):
    def __init__(self):
        CastOpT.__init__(self)
        self.sty = 'ZERO'

    def to_JSON(self):
        return CastOpT.to_JSON(self)

class SIGN(CastOpT):
    def __init__(self):
        CastOpT.__init__(self)
        self.sty = 'SIGN'

    def to_JSON(self):
        return CastOpT.to_JSON(self)

class Expr(Base):
    def __init__(self):
        Base.__init__(self)

    def to_JSON(self):
        args = [arg.to_JSON() for arg in self.args]
        return JSON_converter.to_JSON('Expr', self.sty, args)

class Num(Expr):
    def __init__(self, value, size):
        Expr.__init__(self)
        self.sty = 'Num'
        self.value = value
        self.size = size

    def to_JSON(self):
        self.args = [self.value, self.size]
        return Expr.to_JSON(self)

class Var(Expr):
    def __init__(self, name, size):
        Expr.__init__(self)
        self.sty = 'Var'
        self.name = name
        self.size = size

    def to_JSON(self):
        self.args = [self.name, self.size]
        return Expr.to_JSON(self)

class Load(Expr):
    def __init__(self, addr, size):
        Expr.__init__(self)
        self.sty = 'Load'
        self.addr = addr
        self.size = size

    def to_JSON(self):
        self.args = [self.addr, self.size]
        return Expr.to_JSON(self)

class UnOp(Expr):
    def __init__(self, op, expr):
        Expr.__init__(self)
        self.sty = 'UnOp'
        self.op = op
        self.expr = expr

    def to_JSON(self):
        self.args = [self.op, self.expr]
        return Expr.to_JSON(self)

class BinOp(Expr):
    def __init__(self, op, expr1, expr2):
        Expr.__init__(self)
        self.sty = 'BinOp'
        self.op = op
        self.expr1 = expr1
        self.expr2 = expr2

    def to_JSON(self):
        self.args = [self.op, self.expr1, self.expr2]
        return Expr.to_JSON(self)

class RelOp(Expr):
    def __init__(self, op, expr1, expr2):
        Expr.__init__(self)
        self.sty = 'RelOp'
        self.op = op
        self.expr1 = expr1
        self.expr2 = expr2

    def to_JSON(self):
        self.args = [self.op, self.expr1, self.expr2]
        return Expr.to_JSON(self)

class Cast(Expr):
    def __init__(self, op, size, expr):
        Expr.__init__(self)
        self.sty = 'Cast'
        self.op = op
        self.size = size
        self.expr = expr

    def to_JSON(self):
        self.args = [self.op, self.size, self.expr]
        return Expr.to_JSON(self)

class Ite(Expr):
    def __init__(self, cond, thenExpr, elseExpr):
        Expr.__init__(self)
        self.sty = 'Ite'
        self.cond = cond
        self.thenExpr = thenExpr
        self.elseExpr = elseExpr

    def to_JSON(self):
        self.args = [self.cond, self.thenExpr, self.elseExpr]
        return Expr.to_JSON(self)

class Undefined(Expr):
    def __init__(self):
        Expr.__init__(self)
        self.sty = 'Undefined'

    def to_JSON(self):
        return Expr.to_JSON(self)

class Stmt(Base):
    def __init__(self):
        Base.__init__(self)

    def to_JSON(self):
        args = [arg.to_JSON() for arg in self.args]
        return JSON_converter.to_JSON('Stmt', self.sty, args)

class Start(Stmt):
    def __init__(self, addr, len, endian):
        Stmt.__init__(self)
        self.sty = 'Start'
        self.addr = addr
        self.len = len
        self.endian = endian

    def to_JSON(self):
        self.args = [self.addr, self.len, self.endian]
        return Stmt.to_JSON(self)

class Move(Stmt):
    def __init__(self, name, size, expr):
        Stmt.__init__(self)
        self.sty = 'Move'
        self.name = name
        self.size = size
        self.expr = expr

    def to_JSON(self):
        self.args = [self.name, self.size, self.expr]
        return Stmt.to_JSON(self)

class Store(Stmt):
    def __init__(self, addr, expr):
        Stmt.__init__(self)
        self.sty = 'Store'
        self.addr = addr
        self.expr = expr

    def to_JSON(self):
        self.args = [self.addr, self.expr]
        return Stmt.to_JSON(self)

class Label(Stmt):
    def __init__(self, lbl):
        Stmt.__init__(self)
        self.sty = 'Label'
        self.lbl = lbl

    def to_JSON(self):
        self.args = [self.lbl]
        return Stmt.to_JSON(self)

class CJump(Stmt):
    def __init__(self, cond, thenLbl, elseLbl):
        Stmt.__init__(self)
        self.sty = 'CJump'
        self.cond = cond
        self.thenLbl = thenLbl
        self.elseLbl = elseLbl

    def to_JSON(self):
        self.args = [self.cond, self.thenLbl, self.elseLbl]
        return Stmt.to_JSON(self)

class End(Stmt):
    def __init__(self, addr):
        Stmt.__init__(self)
        self.sty = 'End'
        self.addr = addr

    def to_JSON(self):
        self.args = [self.addr]
        return Stmt.to_JSON(self)

class Unrecognized(Stmt):
    def __init__(self):
        Stmt.__init__(self)
        self.sty = 'Unrecognized'

    def to_JSON(self):
        return Stmt.to_JSON(self)

class AST(Base):
    def __init__(self):
        Base.__init__(self)

    def to_JSON(self):
        args = [arg.to_JSON() for arg in self.args]
        return JSON_converter.to_JSON('AST', self.sty, args)

class Stmts(AST):
    def __init__(self, stmts):
        AST.__init__(self)
        self.sty = 'Stmts'
        self.stmts = stmts

    def to_JSON(self):
        self.args = self.stmts
        return AST.to_JSON(self)

class Uninterpretable(AST):
    def __init__(self):
        AST.__init__(self)
        self.sty = 'Uninterpretable'

    def to_JSON(self):
        return AST.to_JSON(self)

class Incapable(AST):
    def __init__(self):
        AST.__init__(self)
        self.sty = 'Incapable'

    def to_JSON(self):
        return AST.to_JSON(self)
