
import re

from memsim import parser
from memsim.process import AccessType
from memsim.benchmarks import base


TRACE_SUFFIX = '.trace'


class Trace(base.Benchmark):
    """Benchmark to replay an address trace."""

    def __init__(self, name):
        base.Benchmark.__init__(self)
        self.name = name
        self.expr = re.compile(r'([RWMIPCX])([0-9a-fA-F]+):([0-9a-fA-F]+)')

    def __str__(self):
        return ''.join(['(trace (name ', self.name, '))'])

    def skip(self, n):
        assert(n == self.off)
        return True

    def run(self):
        base_name = '/'.join([self.directory, self.name])
        if self.off != 0:
            file_name = ''.join([base_name, '-', str(self.on),
                                '-', str(self.off), TRACE_SUFFIX])
        else:
            file_name = ''.join([base_name, TRACE_SUFFIX])
        with open(file_name, 'r') as f:
            for line in f:
                for m in self.expr.finditer(line):
                    at = m.group(1)
                    addr = int(m.group(2), 16)
                    size = int(m.group(3), 16)
                    if at == 'R':
                        yield AccessType.READ, addr, size
                    elif at == 'W':
                        yield AccessType.WRITE, addr, size
                    elif at == 'M':
                        yield AccessType.READ, addr, size
                        yield AccessType.WRITE, addr, size
                    elif at == 'I':
                        yield AccessType.IDLE, addr, size
                    elif at == 'P':
                        yield AccessType.PRODUCE, addr, size
                    elif at == 'C':
                        yield AccessType.CONSUME, addr, size
                    elif at == 'X':
                        yield AccessType.END, addr, size


def _create_trace(lexer, args):
    name = parser.get_argument(lexer, args, 'name', 'input')
    return Trace(name)
base.constructors['trace'] = _create_trace
