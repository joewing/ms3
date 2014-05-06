from memsim import parser
from memsim.memory import base, main


class RAM(main.MainMemory):

    def __init__(self, word_size=4, latency=100, burst=0):
        main.MainMemory.__init__(self)
        self.word_size = word_size
        self.latency = latency
        self.burst = burst

    def __str__(self):
        result = '(ram '
        result += '(word_size ' + str(self.word_size) + ')'
        if self.latency > 0:
            result += '(latency ' + str(self.latency) + ')'
        if self.burst != 0:
            result += '(burst ' + str(self.burst) + ')'
        result += ')'
        return result

    def get_word_size(self):
        return self.word_size

    def process(self, start, write, addr, size):
        assert(size > 0)
        self.writes += 1 if write else 0
        offset = addr % self.word_size
        count = (size + self.word_size + offset - 1) // self.word_size
        if self.burst == 0:
            return start + self.latency * count
        else:
            return start + self.latency + self.burst * (count - 1)


def _create_ram(lexer, args):
    word_size = parser.get_argument(lexer, args, 'word_size', 4)
    latency = parser.get_argument(lexer, args, 'latency', 100)
    burst = parser.get_argument(lexer, args, 'burst', 0)
    return RAM(word_size, latency, burst)
base.constructors['ram'] = _create_ram
