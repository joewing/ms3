import re

from memsim import parser
from memsim.access import AccessType
from memsim.benchmarks import base


TRACE_SUFFIX = '.trace'


class Trace(base.Benchmark):
    """Benchmark to replay an address trace."""

    def __init__(self, index, name):
        base.Benchmark.__init__(self, index)
        self.name = name
        self.expr = re.compile(r'([RWMIPCKX])([0-9a-fA-F]+):([0-9a-fA-F]+)')
        self.fd = None

    def __str__(self):
        result = '(trace '
        result += '(id ' + str(self.index) + ')'
        result += '(name ' + self.name + ')'
        result += ')'
        return result

    def run(self):
        if self.fd:
            self.fd.seek(0)
        else:
            base_name = '/'.join([self.directory, self.name])
            file_name = ''.join([base_name, TRACE_SUFFIX])
            self.fd = open(file_name, 'r')
        for line in self.fd:
            for m in self.expr.finditer(line):
                at = m.group(1)
                addr = int(m.group(2), 16)
                size = int(m.group(3), 16)
                if at == 'R':
                    yield AccessType.READ, addr, size
                elif at == 'W':
                    yield AccessType.WRITE, addr, size
                elif at == 'M':
                    yield AccessType.MODIFY, addr, size
                elif at == 'I':
                    yield AccessType.IDLE, addr, size
                elif at == 'P':
                    yield AccessType.PRODUCE, addr, size
                elif at == 'C':
                    yield AccessType.CONSUME, addr, size
                elif at == 'K':
                    yield AccessType.PEEK, addr, size
                elif at == 'X':
                    yield AccessType.END, addr, size


def _create_trace(lexer, args):
    index = parser.get_argument(lexer, args, 'id', 0)
    name = parser.get_argument(lexer, args, 'name', 'input')
    return Trace(index, name)
base.constructors['trace'] = _create_trace
