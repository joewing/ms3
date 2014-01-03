
class FIFO(object):
    """Class to simulate a FIFO between processes."""

    def __init__(self, offset, size):
        """Create a simulated FIFO between processes.

        Arguments:
            offset: The byte offset of the FIFO in main memory.
            size: The sie of the FIFO in bytes.
        """
        self.offset = offset
        self.size = size
        self.machine = None
        self.mem = None
        self.read_ptr = 0
        self.write_ptr = 0
        self.count = 0

    def reset(self, machine, mem):
        self.machine = machine
        self.count = self.size // machine.word_size
        self.mem = mem
        self.read_ptr = 0
        self.write_ptr = 0

    def done(self):
        return self.mem.done()

    def produce(self):
        """Put a value on the FIFO.

        Returns the access time or -1 if the FIFO is full.
        """
        next_write_ptr = (self.write_ptr + 1) % self.count
        if next_write_ptr == self.read_ptr:
            return -1
        else:
            word_size = self.machine.word_size
            addr = self.offset + self.write_ptr * word_size
            self.write_ptr = next_write_ptr
            return self.mem.process(0, True, addr, word_size)

    def consume(self):
        """Remove a value from the FIFO.

        Returns the access time or -1 if the FIFO is empty.
        """
        if self.read_ptr == self.write_ptr:
            return -1
        else:
            word_size = self.machine.word_size
            addr = self.offset + self.read_ptr * word_size
            self.read_ptr += 1
            return self.mem.process(0, False, addr, word_size)
