from memsim import parser, util
from memsim.memory import base


class FIFO(base.Memory):
    """Container for FIFO memories."""

    def __init__(self, index, mem, item_size, size):
        """Create a memory to be used as a FIFO.

        Arguments:
            index:      A unique identifier for this FIFO.
            mem:        The memory subsystem.
            item_size:  The size of an item in the FIFO.
            size:       The total size of the FIFO in items.
        """
        base.Memory.__init__(self)
        self.index = index
        self.item_size = util.round_power2(item_size)
        self.size = size
        self.mem = mem
        self.offset = 0
        self.read_ptr = 0
        self.write_ptr = 0
        self.used = 0

    def __str__(self):
        result = '(fifo '
        result += '(id ' + str(self.index) + ')'
        result += '(size ' + str(self.size) + ')'
        result += '(item_size ' + str(self.item_size) + ')'
        result += '(memory ' + self.get_next().get_name() + ')'
        result += ')'
        return result

    def get_word_size(self):
        return self.item_size

    def can_remove(self):
        return False

    def can_insert(self):
        return False

    def total_size(self):
        return self.size * self.item_size

    def set_offset(self, offset):
        self.offset = offset

    def reset(self, machine):
        base.Memory.reset(self, machine)
        self.read_ptr = 0
        self.write_ptr = 0
        self.used = 0

    def is_full(self):
        return self.used == self.size

    def is_empty(self):
        return self.used == 0

    def generate(self, gen):
        name = self.get_id()
        oname = gen.generate_next(self.word_size, self.mem)
        gen.declare_signals(name, self.get_word_size())
        gen.add_code(oname + '_addr <= ' + name + '_addr;')
        gen.add_code(oname + '_din <= ' + name + '_din;')
        gen.add_code(name + '_dout <= ' + oname + '_dout;')
        gen.add_code(oname + '_re <= ' + name + '_re;')
        gen.add_code(oname + '_we <= ' + name + '_we;')
        gen.add_code(oname + '_mask <= ' + name + '_mask;')
        gen.add_code(name + '_ready <= ' + oname + '_ready;')
        return name

    def get_next(self):
        return self.mem

    def set_next(self, n):
        self.mem = n

    def get_path_length(self):
        return self.get_next().get_path_length()

    def permute(self, rand, max_cost, max_size):
        max_size += self.size
        if self.size * 2 > max_size:
            self.size //= 2
        elif self.size // 2 < self.item_size:
            self.size *= 2
        elif rand.randint(0, 1) == 0:
            self.size //= 2
        else:
            self.size *= 2
        assert(self.size >= self.item_size)
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
            addr = self.offset + self.write_ptr * self.item_size
            self.write_ptr = (self.write_ptr + 1) % self.size
            self.used += 1
            return base.send_request(self.mem, 0, True, addr, self.item_size)

    def consume(self):
        """Remove a value from the FIFO.

        Returns the access time or -1 if the FIFO is empty.
        """
        if self.used == 0:
            return -1
        else:
            addr = self.offset + self.read_ptr * self.item_size
            self.read_ptr = (self.read_ptr + 1) % self.size
            self.used -= 1
            return base.send_request(self.mem, 0, False, addr, self.item_size)


def _create_fifo(lexer, args):
    index = parser.get_argument(lexer, args, 'id', 0)
    item_size = parser.get_argument(lexer, args, 'item_size', 4)
    size = parser.get_argument(lexer, args, 'size', 1)
    mem = parser.get_argument(lexer, args, 'memory')
    return FIFO(index=index, mem=mem, size=size, item_size=item_size)
base.constructors['fifo'] = _create_fifo
