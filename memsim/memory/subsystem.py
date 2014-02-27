from memsim import parser
from memsim.memory import base


class Subsystem(base.Memory):
    """Container for per-kernel memory subsystems."""

    def __init__(self, index, word_size, mem):
        """Create a memory to be used with a kernel.

        Arguments:
            index:      A unique identifier for this subsystem.
            word_size:  The word size in bytes.
            mem:        The memory subsystem.
        """
        base.Memory.__init__(self)
        self.index = index
        self.word_size = word_size
        self.mem = mem
        self.offset = 0

    def get_id(self):
        return self.mem.get_id()

    def __str__(self):
        result = '(subsystem '
        result += '(id ' + str(self.index) + ')'
        result += '(word_size ' + str(self.word_size) + ')'
        result += '(memory ' + self.get_next().get_name() + ')'
        result += ')'
        return result

    def get_word_size(self):
        return self.word_size

    def can_remove(self):
        return False

    def can_insert(self):
        return False

    def generate(self, gen):
        return gen.generate_next(self.word_size, self.mem)

    def set_offset(self, offset):
        self.offset = offset

    def get_next(self):
        return self.mem

    def set_next(self, n):
        self.mem = n

    def get_path_length(self):
        return self.get_next().get_path_length()

    def process(self, start, write, addr, size):
        return base.send_request(self.mem, start, write, addr, size)

    def done(self):
        return self.mem.done()


def _create_subsystem(lexer, args):
    index = parser.get_argument(lexer, args, 'id', 0)
    word_size = parser.get_argument(lexer, args, 'word_size', 4)
    mem = parser.get_argument(lexer, args, 'memory')
    return Subsystem(word_size=word_size, index=index, mem=mem)
base.constructors['subsystem'] = _create_subsystem
