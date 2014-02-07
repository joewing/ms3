
from memsim import lex
from memsim.parser import parse_arguments, get_argument

class FIFO(object):
    """Class to simulate a FIFO between processes."""

    def __init__(self, index, size, item_size):
        """Create a simulated FIFO between processes.

        Arguments:
            index: A unique identifier for this FIFO.
            size: The size of the FIFO in items.
            item_size: The size of each item in the FIFO in bytes.
        """
        self.index = index
        self.size = size
        self.item_size = item_size
        self.offset = 0
        self.machine = None
        self.mem = None
        self.read_ptr = 0
        self.write_ptr = 0

    def __str__(self):
        result = '(fifo '
        result += '(id ' + str(self.index) + ')'
        result += '(size ' + str(self.size) + ')'
        result += '(item_size ' + str(self.item_size) + ')'
        result += ')'
        return result

    def total_size(self):
        return self.size * self.item_size

    def set_offset(self, offset):
        self.offset = offset

    def reset(self, machine, mem):
        self.machine = machine
        self.mem = mem
        self.read_ptr = 0
        self.write_ptr = 0

    def done(self):
        return self.mem.done()

    def produce(self):
        """Put a value on the FIFO.

        Returns the access time or -1 if the FIFO is full.
        """
        next_write_ptr = (self.write_ptr + 1) % self.size
        if next_write_ptr == self.read_ptr:
            return -1
        else:
            addr = self.offset + self.write_ptr * self.item_size
            self.write_ptr = next_write_ptr
            return self.mem.process(0, True, addr, self.item_size)

    def consume(self):
        """Remove a value from the FIFO.

        Returns the access time or -1 if the FIFO is empty.
        """
        if self.read_ptr == self.write_ptr:
            return -1
        else:
            addr = self.offset + self.read_ptr * self.item_size
            self.read_ptr += 1
            return self.mem.process(0, False, addr, self.item_size)


def parse_fifo(lexer):
    args = parse_arguments(lexer)
    index = get_argument(lexer, args, 'id', 0)
    size = get_argument(lexer, args, 'size', 1024)
    item_size = get_argument(lexer, args, 'item_size', 4)
    return FIFO(index=index, size=size, item_size=item_size)
