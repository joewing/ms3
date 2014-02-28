from memsim import parser, util
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

    def get_main(self):
        return self

    def set_main(self, mem):
        return mem

    def generate(self, gen, source):
        name = gen.get_name(source, self)
        gen.declare_signals(name, self.get_word_size())
        return name

    def process(self, start, write, addr, size):
        assert(False)


def _create_main(lexer, args):
    if parser.has_argument(lexer, args, 'memory'):
        return parser.get_argument(lexer, args, 'memory')
    else:
        return None
base.constructors['main'] = _create_main
