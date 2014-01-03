
from memsim.memory import base


class MainMemory(base.Memory):
    """Class to represent a main memory."""

    def __init__(self):
        base.Memory.__init__(self)

    def set_main(self, mem):
        return mem

    def get_ports(self, mach):
        name = self.get_id()
        word_size = mach.word_size
        addr_width = mach.addr_bits
        return [base.MemoryPort(name, word_size, addr_width)]

    def generate(self, gen, mach):
        name = self.get_id()
        gen.declare_signals(name, mach.word_size)
