
from memsim import parser
from memsim.memory import base, main


class RAM(main.MainMemory):

    def __init__(self, latency=100, burst=0):
        main.MainMemory.__init__(self)
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

    def process(self, start, write, addr, size):
        assert(size > 0)
        word_size = self.machine.word_size
        offset = addr % word_size
        count = (size + word_size + offset - 1) // word_size
        if self.burst == 0:
            return start + self.latency * count
        else:
            return start + self.latency + self.burst * (count - 1)


def _create_ram(lexer, args):
    latency = parser.get_argument(lexer, args, 'latency', 100)
    return RAM(latency)
base.constructors['ram'] = _create_ram
