import archinfo
import pyvex
from Errors import *
from JSON_converter import *
from Op import *
from UIR import *

name_size_pair32 = {
    'ah' : 1, 'al' : 1, 'ax' : 2, 'eax' : 4,
    'ch' : 1, 'cl' : 1, 'cx' : 2, 'ecx' : 4,
    'dh' : 1, 'dl' : 1, 'dx' : 2, 'edx' : 4,
    'bh' : 1, 'bl' : 1, 'bx' : 2, 'ebx' : 4,
    'esp' : 4, 'sp' : 2, 'bp' : 2, 'ebp' : 4,
    'sih' : 1, 'sil' : 1, 'si' : 2, 'esi' : 4,
    'dih' : 1, 'dil' : 1, 'di' : 2, 'edi' : 4,
    'cc_op' : 4, 'cc_dep1' : 4, 'cc_dep2' : 4, 'cc_ndep' : 4,
    'd' : 4, 'dflag' : 4, 'id' : 4, 'idflag' : 4, 'ac' : 4, 'acflag' : 4,
    'eip' : 4, 'ip' : 4, 'pc' : 4,
    'mm0' : 8, 'mm1' : 8, 'mm2' : 8, 'mm3' : 8,
    'mm4' : 8, 'mm5' : 8, 'mm6' : 8, 'mm7' : 8,
    'fpreg' : 64, 'fpu_regs' : 64, 'fptag' : 8,
    'fpu_tags' : 8, 'fpround' : 4, 'fc3210' : 4, 'ftop' : 4, 'sseround' : 4,
    'xmm0' : 16, 'xmm1' : 16, 'xmm2' : 16, 'xmm3' : 16,
    'xmm4' : 16, 'xmm5' : 16, 'xmm6' : 16, 'xmm7' : 16,
    'cs' : 2, 'ds' : 2, 'es' : 2, 'fs' : 2, 'gs' : 2, 'ss' : 2,
    'ldt' : 8, 'gdt' : 8, 'emnote' : 4, 'cmstart' : 4, 'cmlen' : 4,
    'nraddr' : 4, 'sc_class' : 4, 'ip_at_syscall' : 4,
}

name_size_pair64 = {
    'ah' : 1, 'al' : 1, 'ax' : 2, 'eax' : 4, 'rax' : 8,
    'ch' : 1, 'cl' : 1, 'cx' : 2, 'ecx' : 4, 'rcx' : 8,
    'dh' : 1, 'dl' : 1, 'dx' : 2, 'edx' : 4, 'rdx' : 8,
    'bh' : 1, 'bl' : 1, 'bx' : 2, 'ebx' : 4, 'rbx' : 8,
    'esp' : 4, 'rsp' : 8, 'sp' : 2, 'bp' : 2, 'ebp' : 4, 'rbp' : 8,
    'sih' : 1, 'sil' : 1, 'si' : 2, 'esi' : 4, 'rsi' : 8,
    'dih' : 1, 'dil' : 1, 'di' : 2, 'edi' : 4, 'rdi' : 8,
    'r8' : 8, 'r9' : 8, 'r10' : 8, 'r11' : 8,
    'r12' : 8, 'r13' : 8, 'r14' : 8, 'r15' : 8,
    'cc_op' : 8, 'cc_dep1' : 8, 'cc_dep2' : 8, 'cc_ndep' : 8,
    'd' : 8, 'dflag' : 8, 'idflag' : 8, 'acflag' : 8,
    'rip' : 8, 'ip' : 8, 'pc' : 8,
    'mm0' : 8, 'mm1' : 8, 'mm2' : 8, 'mm3' : 8,
    'mm4' : 8, 'mm5' : 8, 'mm6' : 8, 'mm7' : 8,
    'fpreg' : 64, 'fpu_regs' : 64, 'fptag' : 8, 'fs_const' : 8,
    'fpu_tags' : 8, 'fpround' : 4, 'fc3210' : 4, 'ftop' : 4, 'sseround' : 8,
    'xmm0' : 16, 'xmm1' : 16, 'xmm2' : 16, 'xmm3' : 16,
    'xmm4' : 16, 'xmm5' : 16, 'xmm6' : 16, 'xmm7' : 16,
    'xmm8' : 16, 'xmm9' : 16, 'xmm10' : 16, 'xmm11' : 16,
    'xmm12' : 16, 'xmm13' : 16, 'xmm14' : 16, 'xmm15' : 16,
    'ymm0' : 32, 'ymm1' : 32, 'ymm2' : 32, 'ymm3' : 32,
    'ymm4' : 32, 'ymm5' : 32, 'ymm6' : 32, 'ymm7' : 32,
    'ymm8' : 32, 'ymm9' : 32, 'ymm10' : 32, 'ymm11' : 32,
    'ymm12' : 32, 'ymm13' : 32, 'ymm14' : 32, 'ymm15' : 32,
    'cs' : 2, 'ds' : 2, 'es' : 2, 'fs' : 2, 'gs' : 2, 'ss' : 2,
    'ldt' : 8, 'gdt' : 8, 'emnote' : 4, 'cmstart' : 4, 'cmlen' : 4,
    'nraddr' : 4, 'sc_class' : 4, 'ip_at_syscall' : 4,
}

class Lifter:
    def __init__(self, arch):
        if arch == 'x86':
            self.arch = archinfo.ArchX86()
            self.addr = 0x8048000
            self.name_size_pair = name_size_pair32
        elif arch == 'x64':
            self.arch = archinfo.ArchAMD64()
            self.addr = 0x401000
            self.name_size_pair = name_size_pair64
        self.statements = []
        self.lbl = 0

        self.ftop = False

        self.cc_op = None
        self.cc_dep1 = None
        self.cc_dep2 = None
        self.cc_ndep = None

    def lift(self, insn):
        try:
            self.irsb = pyvex.IRSB(insn.decode('hex'), self.addr, self.arch)
            if self.irsb.instructions != 1:
                raise InvalidInstruction
            if self.irsb.jumpkind == 'Ijk_NoDecode':
                return JSONize(Uninterpretable().to_JSON())
            self.tyenv = self.irsb.tyenv
        except pyvex.errors.PyVEXError:
            return JSONize(Uninterpretable().to_JSON())

        try:
            for stmt in self.irsb.statements:
                parsed = self.fetch_stmt(stmt)
                self.deal_stmt(parsed)

            if isinstance(self.irsb.next, pyvex.expr.RdTmp):
                tmp = self.irsb.next.tmp
                name = Basic('t%d' % tmp)
                size = Basic(self._get_size(self.irsb.next))
                addr = Var(name, size)
            else:
                addr = Number(self.irsb.next.con.value, 32)
            self.statements.append(End(addr))
            return JSONize(Stmts(self.statements).to_JSON())
        except IncapableError:
            return JSONize(Incapable().to_JSON())

    def deal_stmt(self, parsed):
        ty = parsed['ty']
        if ty == 'IMark':
            addr = Basic(parsed['addr'])
            len = Basic(parsed['len'])
            endian = LE()
            stmt = Start(addr, len, endian)
            self.statements.append(stmt)
        elif ty == 'Put':
            if parsed['name'] == 'ftop':
                self.ftop = True
            elif parsed['name'].startswith('cc_'):
                if parsed['name'] == 'cc_op':
                    self.cc_op = parsed['expr']['expr']
                elif parsed['name'] == 'cc_dep1':
                    self.cc_dep1 = parsed['expr']['expr']
                elif parsed['name'] == 'cc_dep2':
                    self.cc_dep2 = parsed['expr']['expr']
                elif parsed['name'] == 'cc_ndep':
                    self.cc_ndep = parsed['expr']['expr']
                if self.check_eflags_cond():
                    statements = self.calc_eflags()
                    self.cc_op = None
                    self.cc_dep1 = None
                    self.cc_dep2 = None
                    self.cc_ndep = None
                    self.statements += statements
            else:
                name = Basic(parsed['name'])
                size = Basic(parsed['size'])
                stmt = Move(name, size, parsed['expr']['expr'])
                self.statements.append(stmt)
        elif ty == 'PutI':
            if self.ftop == False:
                raise NotImplemented
        elif ty == 'WrTmp':
            name = Basic(parsed['name'])
            size = Basic(parsed['size'])
            if parsed['expr']['ty'] == 'CCall':
                if parsed['expr']['name'] == 'x86g_calculate_eflags_c':
                    expr = Zero(32, Var(Basic('cf'), Basic('1')))
                elif parsed['expr']['name'] == 'x86g_calculate_eflags_all':
                    cf = Zero(32, Var(Basic('cf'), Basic('1')))
                    pf = Zero(32, Var(Basic('pf'), Basic('1')))
                    af = Zero(32, Var(Basic('af'), Basic('1')))
                    zf = Zero(32, Var(Basic('zf'), Basic('1')))
                    sf = Zero(32, Var(Basic('sf'), Basic('1')))
                    of = Zero(32, Var(Basic('of'), Basic('1')))
                    pf = Shl_n(2, 32, pf)
                    af = Shl_n(4, 32, af)
                    zf = Shl_n(6, 32, zf)
                    sf = Shl_n(7, 32, sf)
                    of = Shl_n(11, 32, of)
                    expr = Or(Or(Or(cf, pf), af), Or(Or(zf, sf), of))
                elif parsed['expr']['name'] == 'x86g_calculate_daa_das_aaa_aas':
                    raise IncapableError
                elif parsed['expr']['name'] == 'x86g_calculate_condition':
                    raise IncapableError
                elif parsed['expr']['name'] == 'amd64g_calculate_rflags_all':
                    cf = Zero(64, Var(Basic('cf'), Basic('1')))
                    pf = Zero(64, Var(Basic('pf'), Basic('1')))
                    af = Zero(64, Var(Basic('af'), Basic('1')))
                    zf = Zero(64, Var(Basic('zf'), Basic('1')))
                    sf = Zero(64, Var(Basic('sf'), Basic('1')))
                    of = Zero(64, Var(Basic('of'), Basic('1')))
                    pf = Shl_n(2, 64, pf)
                    af = Shl_n(4, 64, af)
                    zf = Shl_n(6, 64, zf)
                    sf = Shl_n(7, 64, sf)
                    of = Shl_n(11, 64, of)
                    expr = Or(Or(Or(cf, pf), af), Or(Or(zf, sf), of))
                elif parsed['expr']['name'] == 'amd64g_calculate_rflags_c':
                    expr = Zero(64, Var(Basic('cf'), Basic('1')))
            else:
                if isinstance(parsed['expr']['expr'], Var):
                    if parsed['expr']['expr'].name.value == 'ftop':
                        self.ftop = True
                        return
                expr = parsed['expr']['expr']
            stmt = Move(name, size, expr)
            self.statements.append(stmt)
        elif ty == 'Store':
            stmt = Store(parsed['addr']['expr'], parsed['data']['expr'])
            self.statements.append(stmt)
        elif ty == 'Exit':
            thenLbl = Basic('Label%d' % self.lbl)
            self.lbl += 1
            elseLbl = Basic('Label%d' % self.lbl)
            self.lbl += 1
            stmt = CJump(parsed['cond']['expr'], thenLbl, elseLbl)
            self.statements.append(stmt)
            self.statements.append(Label(thenLbl))
            addr = Number(parsed['addr'], 32)
            stmt = End(addr)
            self.statements.append(stmt)
            self.statements.append(Label(elseLbl))
        elif ty == 'CAS':
            addr = parsed['addr']['expr']
            expd = parsed['expd']['expr']
            expr = parsed['expr']['expr']
            tmp = parsed['tmp']
            size = parsed['size']
            cond = Eq(Load(addr, Basic(size)), expd)
            thenLbl = Basic('Label%d' % self.lbl)
            self.lbl += 1
            elseLbl = Basic('Label%d' % self.lbl)
            self.lbl += 1
            stmt = CJump(cond, thenLbl, elseLbl)
            self.statements.append(stmt)
            self.statements.append(Label(thenLbl))
            stmt = Store(addr, expr)
            self.statements.append(stmt)
            lbl = Basic('Label%d' % self.lbl)
            self.lbl += 1
            self.statements.append(Label(elseLbl))
            name = 't%d' % tmp
            stmt = Move(Basic(name), Basic(size), expr)
            self.statements.append(stmt)
        elif ty == 'AbiHint':
            return
        else:
            print ty
            raise NotImplemented

    def fetch_stmt(self, stmt):
        res = {}
        if isinstance(stmt, pyvex.stmt.NoOp):
            raise NotImplemented
        elif isinstance(stmt, pyvex.stmt.IMark):
            res['ty'] = 'IMark'
            addr = stmt.addr
            res['addr'] = addr
            len = stmt.len
            res['len'] = len
        elif isinstance(stmt, pyvex.stmt.AbiHint):
            res['ty'] = 'AbiHint'
        elif isinstance(stmt, pyvex.stmt.Put):
            res['ty'] = 'Put'
            offset = stmt.offset
            data = stmt.data
            size = self._get_size(data)
            name = self.arch.translate_register_name(offset, size / 8)
            if self.addr == 0x8048000 and name == '168':
                res['name'] = 'xmm0'
                res['size'] = 128
                expr = self.fetch_expr(data)['expr']
                var = Var(Basic('xmm0'), Basic(128))
                var = Zero(128, Low(64, var))
                expr = Or(var, Shl_n(64, 128, Zero(128, expr)))
                res['expr'] = {'ty' : 'Binop', 'expr' : expr}
            else:
                orig_size = self.name_size_pair[name] * 8
                res['name'] = name
                res['size'] = orig_size
                if size < orig_size:
                    var = Var(Basic(name), Basic(orig_size))
                    expr = Zero(orig_size, High(orig_size - size, var))
                    expr = Shl_n(size, orig_size, expr)
                    expr = Or(expr, Zero(orig_size, self.fetch_expr(data)['expr']))
                    res['expr'] = {'ty' : 'Binop', 'expr' : expr}
                else:
                    res['expr'] = self.fetch_expr(data)
        elif isinstance(stmt, pyvex.stmt.PutI):
            res['ty'] = 'PutI'
        elif isinstance(stmt, pyvex.stmt.WrTmp):
            res['ty'] = 'WrTmp'
            tmp = stmt.tmp
            data = stmt.data
            size = self._get_size(data)
            res['name'] = 't%d' % tmp
            res['size'] = size
            res['expr'] = self.fetch_expr(data)
        elif isinstance(stmt, pyvex.stmt.Store):
            res['ty'] = 'Store'
            addr = stmt.addr
            data = stmt.data
            res['addr'] = self.fetch_expr(addr)
            res['data'] = self.fetch_expr(data)
        elif isinstance(stmt, pyvex.stmt.LoadG):
            raise NotImplemented
        elif isinstance(stmt, pyvex.stmt.StoreG):
            raise NotImplemented
        elif isinstance(stmt, pyvex.stmt.CAS):
            res['ty'] = 'CAS'
            addr = stmt.addr
            expd = stmt.expdLo
            expr = stmt.dataLo
            tmp = stmt.oldLo
            size = self._get_size(expr)
            res['addr'] = self.fetch_expr(addr)
            res['expd'] = self.fetch_expr(expd)
            res['expr'] = self.fetch_expr(expr)
            res['size'] = size
            res['tmp'] = stmt.oldLo
        elif isinstance(stmt, pyvex.stmt.LLSC):
            raise NotImplemented
        elif isinstance(stmt, pyvex.stmt.Dirty):
            raise IncapableError
        elif isinstance(stmt, pyvex.stmt.MBE):
            raise NotImplemented
        elif isinstance(stmt, pyvex.stmt.Exit):
            res['ty'] = 'Exit'
            cond = stmt.guard
            addr = stmt.dst.value
            res['cond'] = self.fetch_expr(cond)
            res['addr'] = addr
        else:
            raise NotImplemented
        return res


    def fetch_expr(self, expr):
        res = {}
        if isinstance(expr, pyvex.expr.Binder):
            raise NotImplemented
        elif isinstance(expr, pyvex.expr.Get):
            res['ty'] = 'Get'
            offset = expr.offset
            size = self._get_size(expr)
            name = self.arch.translate_register_name(offset, size / 8)
            if self.addr == 0x8048000 and name == '168':
                res['expr'] = High(64, Var(Basic('xmm0'), Basic(128)))
            elif self.addr == 0x8048000 and name == '184':
                res['expr'] = High(64, Var(Basic('xmm1'), Basic(128)))
            elif self.addr == 0x8048000 and name == '164':
                raise IncapableError
            elif self.addr == 0x8048000 and name == 'd':
                var = Var(Basic('df'), Basic(1))
                num0 = Num(Basic(0), Basic(1))
                num1 = Num(Basic(1), Basic(32))
                num2 = Num(Basic(0xffffffff), Basic(32))
                res['expr'] = Ite(Eq(var, num0), num1, num2)
            elif self.addr == 0x401000 and name == '228':
                raise IncapableError
            elif self.addr == 0x401000 and name == '232':
                res['expr'] = High(64, Var(Basic('xmm0'), Basic(128)))
            elif self.addr == 0x401000 and name == '264':
                res['expr'] = High(64, Var(Basic('xmm1'), Basic(128)))
            else:
                orig_size = self.name_size_pair[name] * 8
                if size < orig_size:
                    res['expr'] = Low(size, Var(Basic(name), Basic(orig_size)))
                else:
                    res['expr'] = Var(Basic(name), Basic(size))
        elif isinstance(expr, pyvex.expr.GetI):
            raise IncapableError
        elif isinstance(expr, pyvex.expr.RdTmp):
            res['ty'] = 'RdTmp'
            tmp = expr.tmp
            name = Basic('t%d' % tmp)
            size = Basic(self._get_size(expr))
            res['expr'] = Var(name, size)
        elif isinstance(expr, pyvex.expr.Qop):
            raise NotImplemented
        elif isinstance(expr, pyvex.expr.Triop):
            raise IncapableError
        elif isinstance(expr, pyvex.expr.Binop):
            res['ty'] = 'Binop'
            expr1 = self.fetch_expr(expr.args[0])
            expr2 = self.fetch_expr(expr.args[1])
            op = expr.op
            res['expr'] = fetch_binop(op, expr1['expr'], expr2['expr'])
        elif isinstance(expr, pyvex.expr.Unop):
            res['ty'] = 'Unop'
            op = expr.op
            expr = self.fetch_expr(expr.args[0])
            res['expr'] = fetch_unop(op, expr['expr'])
        elif isinstance(expr, pyvex.expr.Load):
            res['ty'] = 'Load'
            addr = self.fetch_expr(expr.addr)
            size = Basic(self._get_size(expr))
            res['expr'] = Load(addr['expr'], size)
        elif isinstance(expr, pyvex.expr.Const):
            res['ty'] = 'Const'
            value = Basic(expr.con.value)
            size = Basic(self._get_size(expr))
            res['expr'] = Num(value, size)
        elif isinstance(expr, pyvex.expr.ITE):
            res['ty'] = 'ITE'
            cond = self.fetch_expr(expr.cond)
            thenExpr = self.fetch_expr(expr.iftrue)
            elseExpr = self.fetch_expr(expr.iffalse)
            res['expr'] = Ite(cond['expr'], thenExpr['expr'], elseExpr['expr'])
        elif isinstance(expr, pyvex.expr.CCall):
            res['ty'] = 'CCall'
            name = expr.cee.name
            if name == 'x86g_calculate_mmx_psadbw':
                raise IncapableError
            elif name == 'x86g_calculate_mmx_pmaddwd':
                raise IncapableError
            elif name == 'x86g_calculate_condition':
                res['name'] = name
                res['op'] = expr.args[0].con.value
            elif name == 'x86g_calculate_eflags_c':
                res['name'] = name
            elif name == 'x86g_calculate_eflags_all':
                res['name'] = name
            elif name == 'x86g_calculate_daa_das_aaa_aas':
                raise IncapableError
            elif name == 'amd64g_calculate_mmx_psadbw':
                raise IncapableError
            elif name == 'amd64g_calculate_mmx_pmaddwd':
                raise IncapableError
            elif name == 'amd64g_calculate_condition':
                raise IncapableError
            elif name == 'amd64g_calculate_rflags_all':
                res['name'] = name
            elif name == 'amd64g_calculate_rflags_c':
                res['name'] = name
            elif name == 'x86g_use_seg_selector':
                raise IncapableError
            else:
                print name
                raise NotImplemented
        elif isinstance(expr, pyvex.expr.GSPTR):
            raise NotImplemented
        elif isinstance(expr, pyvex.expr.VECRET):
            raise NotImplemented
        else:
            raise NotImplemented
        return res

    def _get_size(self, data):
        return data.result_size(self.tyenv)

    def check_eflags_cond(self):
        return None not in [self.cc_op, self.cc_dep1,
                            self.cc_dep2, self.cc_ndep]

    def calc_eflags(self):
        if isinstance(self.cc_op, Num):
            op = self.cc_op.value.value
            size = 8 * (2 ** ((op - 1) % 3))
            op = (op + 2) / 3

            if op == 0:
                flags = eflags32_copy(self.cc_dep1)
            elif op == 1:
                flags = eflags32_add(size, self.cc_dep1, self.cc_dep2)
            elif op == 2:
                flags = eflags32_sub(size, self.cc_dep1, self.cc_dep2)
            elif op == 3:
                flags = eflags32_adc(size, self.cc_dep1,
                                     self.cc_dep2, self.cc_ndep)
            elif op == 4:
                flags = eflags32_sbb(size, self.cc_dep1,
                                     self.cc_dep2, self.cc_ndep)
            elif op == 5:
                flags = eflags32_logic(size, self.cc_dep1)
            elif op == 6:
                flags = eflags32_inc(size, self.cc_dep1)
            elif op == 7:
                flags = eflags32_dec(size, self.cc_dep1)
            elif op == 8:
                flags = eflags32_shl(size, self.cc_dep1, self.cc_dep2)
            elif op == 9:
                flags = eflags32_shr(size, self.cc_dep1, self.cc_dep2)
            elif op == 10:
                flags = eflags32_rol(size, self.cc_dep1, self.cc_ndep)
            elif op == 11:
                flags = eflags32_ror(size, self.cc_dep1, self.cc_ndep)
            elif op == 12:
                flags = eflags32_umul(size, self.cc_dep1, self.cc_dep2)
            elif op == 13:
                flags = eflags32_smul(size, self.cc_dep1, self.cc_dep2)

            names = ['cf', 'pf', 'af', 'zf', 'sf', 'of']
            stmts = []
            for name in names:
                if flags[name] is not None:
                    stmts.append(Move(Basic(name), Basic(1), flags[name]))
            return stmts
        else:
            raise IncapableError
