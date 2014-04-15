from memsim import parser
from memsim.memory import subsystem, base


class FIFO(subsystem.Subsystem):
    """Container for FIFO memories."""

    def __init__(self, index, mem, word_size, depth, min_depth):
        """Create a memory to be used as a FIFO.

        Arguments:
            index:      A unique identifier for this FIFO.
            mem:        The memory subsystem.
            word_size:  The size of an item in the FIFO.
            depth:      The depth of the FIFO in items.
            min_depth:  Minimum depth in items.
        """
        depth = max(depth, min_depth)
        subsystem.Subsystem.__init__(self, index, word_size, depth, mem)
        self.min_depth = min_depth
        self.read_ptr = 0
        self.write_ptr = 0
        self.used = 0

    def __str__(self):
        result = '(fifo '
        result += '(id ' + str(self.index) + ')'
        result += '(depth ' + str(self.depth) + ')'
        if self.min_depth > 1:
            result += '(min_depth ' + str(self.min_depth) + ')'
        result += '(word_size ' + str(self.word_size) + ')'
        result += '(memory ' + self.get_next().get_name() + ')'
        result += ')'
        return result

    def total_size(self):
        return self.depth * self.word_size

    def reset(self, machine):
        subsystem.Subsystem.reset(self, machine)
        self.read_ptr = 0
        self.write_ptr = 0
        self.used = 0

    def is_full(self):
        return self.used == self.depth

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
        max_size += self.total_size()
        if self.total_size() * 2 > max_size:
            if self.depth // 2 < self.min_depth:
                return False
            self.depth //= 2
        elif self.depth <= self.min_depth:
            self.depth *= 2
        elif rand.randint(0, 1) == 0:
            self.depth //= 2
        else:
            self.depth *= 2
        assert(self.depth >= self.min_depth)
        assert(self.total_size() <= max_size)
        return True

    def process(self, start, write, addr, size):
        if self.depth == 1:
            # Single cycle access for 1-deep FIFOs.
            return start + 1
        else:
            return subsystem.Subsystem.process(self, start, write, addr, size)

    def produce(self):
        """Put a value on the FIFO.

        Returns the access time or -1 if the FIFO is full.
        """
        if self.used == self.depth:
            return -1
        else:
            addr = self.write_ptr * self.word_size
            self.write_ptr = (self.write_ptr + 1) % self.depth
            self.used += 1
            return self.process(0, True, addr, self.word_size)

    def consume(self):
        """Remove a value from the FIFO.

        Returns the access time or -1 if the FIFO is empty.
        """
        if self.used == 0:
            return -1
        else:
            addr = self.read_ptr * self.word_size
            self.read_ptr = (self.read_ptr + 1) % self.depth
            self.used -= 1
            return self.process(0, False, addr, self.word_size)

    def peek(self, offset):
        """Peek at a value on the FIFO.

        offset is the offset back from the read pointer in items.
        Returns the access time or -1 if not available.
        """
        if self.used <= offset:
            return -1
        else:
            temp = (self.read_ptr - offset) % self.depth
            addr = temp * self.word_size
            return self.process(0, False, addr, self.word_size)


def _create_fifo(lexer, args):
    index = parser.get_argument(lexer, args, 'id', 0)
    word_size = parser.get_argument(lexer, args, 'word_size', 4)
    depth = parser.get_argument(lexer, args, 'depth', 1)
    min_depth = parser.get_argument(lexer, args, 'min_depth', 1)
    mem = parser.get_argument(lexer, args, 'memory')
    return FIFO(index=index, mem=mem, depth=depth, min_depth=min_depth,
                word_size=word_size)
base.constructors['fifo'] = _create_fifo
