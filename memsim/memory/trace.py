
from __future__ import print_function

from memsim import parser
from memsim.memory import base, container


class Trace(container.Container):

    def __init__(self, mem):
        container.Container.__init__(self, mem)

    def __str__(self):
        return '(trace (memory ' + str(self.mem.get_name()) + '))'

    def generate(self, gen):
        return self.get_next().generate(gen)

    def get_word_size(self):
        return self.mem.get_word_size()

    def process(self, start, write, addr, size):
        out = ['W' if write else 'R']
        out.append('%x' % addr)
        out.append(':')
        out.append('%x' % size)
        print(''.join(out))
        return self.get_next().process(start, write, addr, size)


def _create_trace(lexer, args):
    mem = parser.get_argument(lexer, args, 'memory')
    return Trace(mem=mem)
base.constructors['trace'] = _create_trace
