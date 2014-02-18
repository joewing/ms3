from memsim import parser
from memsim.memory import base


class MainMemory(base.Memory):
    """Class to represent a main memory."""

    def __init__(self):
        base.Memory.__init__(self)

    def get_name(self):
        return '(main)'

    def can_remove(self):
        return False

    def can_insert(self):
        return True

    def set_main(self, mem):
        return mem

    def get_ports(self, mach):
        name = self.get_id()
        word_size = mach.word_size
        addr_width = mach.addr_bits - mach.word_bits
        return [base.MemoryPort(name, word_size, addr_width)]

    def generate(self, gen, mach):
        name = self.get_id()
        gen.declare_signals(name, mach.word_size)

    def process(self, start, write, addr, size):
        assert(False)


def _create_main(lexer, args):
    if parser.has_argument(lexer, args, 'memory'):
        return parser.get_argument(lexer, args, 'memory')
    else:
        return MainMemory()
base.constructors['main'] = _create_main
