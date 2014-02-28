from memsim import parser
from memsim.memory import subsystem, base


class FIFO(subsystem.Subsystem):
    """Container for FIFO memories."""

    def __init__(self, index, mem, word_size, size):
        """Create a memory to be used as a FIFO.

        Arguments:
            index:      A unique identifier for this FIFO.
            mem:        The memory subsystem.
            word_size:  The size of an item in the FIFO.
            size:       The total size of the FIFO in items.
        """
        subsystem.Subsystem.__init__(self, index, word_size, mem)
        self.size = size
        self.read_ptr = 0
        self.write_ptr = 0
        self.used = 0

    def __str__(self):
        result = '(fifo '
        result += '(id ' + str(self.index) + ')'
        result += '(size ' + str(self.size) + ')'
        result += '(word_size ' + str(self.word_size) + ')'
        result += '(memory ' + self.get_next().get_name() + ')'
        result += ')'
        return result

    def total_size(self):
        return self.size * self.word_size

    def reset(self, machine):
        subsystem.Subsystem.reset(self, machine)
        self.read_ptr = 0
        self.write_ptr = 0
        self.used = 0

    def is_full(self):
        return self.used == self.size

    def is_empty(self):
        return self.used == 0

    def generate(self, gen, source):
        name = gen.get_name(source, self)
        word_size = self.get_word_size()
        oname = gen.generate_next(self, self.mem)
        gen.declare_signals(name, word_size)
        gen.add_code(oname + '_addr <= ' + name + '_addr;')
        gen.add_code(oname + '_din <= ' + name + '_din;')
        gen.add_code(name + '_dout <= ' + oname + '_dout;')
        gen.add_code(oname + '_re <= ' + name + '_re;')
        gen.add_code(oname + '_we <= ' + name + '_we;')
        gen.add_code(oname + '_mask <= ' + name + '_mask;')
        gen.add_code(name + '_ready <= ' + oname + '_ready;')
        return name

    def permute(self, rand, max_cost, max_size):
        max_size += self.size
        if self.size * 2 > max_size:
            self.size //= 2
        elif self.size // 2 < self.word_size:
            self.size *= 2
        elif rand.randint(0, 1) == 0:
            self.size //= 2
        else:
            self.size *= 2
        assert(self.size >= self.word_size)
        assert(self.size <= max_size)
        return True

    def process(self, start, write, addr, size):
        return self.mem.process(start, write, addr, size)

    def done(self):
        return self.mem.done()

    def produce(self):
        """Put a value on the FIFO.

        Returns the access time or -1 if the FIFO is full.
        """
        if self.used == self.size:
            return -1
        else:
            addr = self.offset + self.write_ptr * self.word_size
            self.write_ptr = (self.write_ptr + 1) % self.size
            self.used += 1
            return base.send_request(self.mem, 0, True, addr, self.word_size)

    def consume(self):
        """Remove a value from the FIFO.

        Returns the access time or -1 if the FIFO is empty.
        """
        if self.used == 0:
            return -1
        else:
            addr = self.offset + self.read_ptr * self.word_size
            self.read_ptr = (self.read_ptr + 1) % self.size
            self.used -= 1
            return base.send_request(self.mem, 0, False, addr, self.word_size)


def _create_fifo(lexer, args):
    index = parser.get_argument(lexer, args, 'id', 0)
    word_size = parser.get_argument(lexer, args, 'word_size', 4)
    size = parser.get_argument(lexer, args, 'size', 1)
    mem = parser.get_argument(lexer, args, 'memory')
    return FIFO(index=index, mem=mem, size=size, word_size=word_size)
base.constructors['fifo'] = _create_fifo
