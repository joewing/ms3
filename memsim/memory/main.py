from memsim import parser
from memsim.memory import base


class MainMemory(base.Memory):
    """Class to represent a main memory."""

    def __init__(self):
        base.Memory.__init__(self)

    def get_name(self, full):
        assert(full == False)
        return '(main)'

    def get_parameter_count(self):
        return 0

    def can_remove(self):
        return False

    def can_insert(self):
        return True

    def get_cost(self):
        return self.machine.get_zero_cost()

    def get_main(self):
        return self

    def set_main(self, mem):
        return mem

    def generate(self, gen, source):
        name = gen.get_name(source, self)
        gen.declare_signals(name, self.get_word_size())
        return name

    def reset(self, machine):
        base.Memory.reset(self, machine)


def _create_main(lexer, args):
    if parser.has_argument(lexer, args, 'memory'):
        return parser.get_argument(lexer, args, 'memory')
    else:
        return None
base.constructors['main'] = _create_main
