
from .. import parser
from . import base


class RAM(base.Memory):

    def __init__(self, latency=100, burst=0):
        base.Memory.__init__(self)
        self.latency = latency
        self.burst = burst

    def __str__(self):
        result = '(ram'
        space = ' '
        if self.latency > 0:
            result += space + '(latency ' + str(self.latency) + ')'
            space = ''
        if self.burst != 0:
            result += space + '(burst ' + str(self.burst) + ')'
            space = ''
        result += ')'
        return result

    def get_ports(self, mach):
        name = self.get_id()
        word_size = mach.word_size
        addr_width = mach.addr_bits
        return [base.MemoryPort(name, word_size, addr_width)]

    def generate(self, gen, mach):
        name = self.get_id()
        gen.declare_signals(name, mach.word_size)

    def process(self, start, write, addr, size):
        assert(size > 0)
        word_size = self.machine.word_size
        offset = addr % word_size
        count = (size + word_size + offset - 1) // word_size
        if self.burst == 0:
            return start + count * self.latency
        else:
            return start + self.latency + self.burst * (count - 1)


def _create_ram(lexer, args):
    latency = parser.get_argument(lexer, args, 'latency', 100)
    return RAM(latency)
base.constructors['ram'] = _create_ram
