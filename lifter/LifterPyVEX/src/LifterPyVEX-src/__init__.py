from .JSON_converter import to_JSON
from .Lifter import Lifter

ARCH = ['x86', 'x64']
HEXSTR = '0123456789abcdef'

def sanitize_hexstr(s):
    if len(s) % 2 != 0:
        return False

    for c in s.lower():
        if c not in HEXSTR:
            return False

    return True

def lift(arch, insn):
    if arch not in ARCH:
        return to_JSON('AST', 'Incapable', [])
    if not sanitize_hexstr(insn):
        return to_JSON('AST', 'Incapable', [])

    return Lifter(arch).lift(insn)
