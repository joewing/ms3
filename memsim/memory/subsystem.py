from memsim import parser, util
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
        self.word_size = util.round_power2(word_size)
        self.mem = mem
        self.offset = 0
        self.score = 0

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

    def generate(self, gen, source):
        name = gen.get_name(source, self)
        oname = gen.generate_next(self, self.mem)
        gen.declare_signals(name, self.get_word_size())
        gen.add_code(oname + '_addr <= ' + name + '_addr;')
        gen.add_code(oname + '_din <= ' + name + '_din;')
        gen.add_code(name + '_dout <= ' + oname + '_dout;')
        gen.add_code(oname + '_re <= ' + name + '_re;')
        gen.add_code(oname + '_we <= ' + name + '_we;')
        gen.add_code(oname + '_mask <= ' + name + '_mask;')
        gen.add_code(name + '_ready <= ' + oname + '_ready;')
        return name

    def set_offset(self, offset):
        self.offset = offset

    def get_next(self):
        return self.mem

    def set_next(self, n):
        self.mem = n

    def get_path_length(self):
        word_size = self.word_size
        next_word_size = self.mem.get_word_size()
        length = self.mem.get_path_length()
        if word_size != next_word_size:
            length += self.machine.addr_bits
        return length

    def reset(self, machine):
        base.Memory.reset(self, machine)
        self.score = 0

    def process(self, start, write, addr, size):
        result = base.send_request(self.mem, start, write, addr, size)
        self.score += result
        return result

    def done(self):
        result = self.mem.done()
        self.score += result
        return result


def _create_subsystem(lexer, args):
    index = parser.get_argument(lexer, args, 'id', 0)
    word_size = parser.get_argument(lexer, args, 'word_size', 4)
    mem = parser.get_argument(lexer, args, 'memory')
    return Subsystem(word_size=word_size, index=index, mem=mem)
base.constructors['subsystem'] = _create_subsystem
