import re

from memsim import parser
from memsim.access import AccessType
from memsim.benchmarks import base


TRACE_SUFFIX = '.trace'


class Trace(base.Benchmark):
    """Benchmark to replay an address trace."""

    def __init__(self, index, name, last):
        base.Benchmark.__init__(self, index, last)
        self.name = name
        self.expr = re.compile(r'([RWMIPCKX])([0-9a-fA-F]+):([0-9a-fA-F]+)')

    def __str__(self):
        result = '(trace '
        result += '(id ' + str(self.index) + ')'
        if self.last:
            result += '(last true)'
        result += '(name ' + self.name + ')'
        result += ')'
        return result

    def run(self, repeat):
        base_name = '/'.join([self.directory, self.name])
        file_name = ''.join([base_name, TRACE_SUFFIX])
        while True:
            with open(file_name, 'r') as fd:
                for line in fd:
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
                        elif at == 'A':
                            yield AccessType.INPUT, addr, size
                        elif at == 'O':
                            yield AccessType.OUTPUT, addr, size
                        elif at == 'X':
                            yield AccessType.END, addr, size
                        else:
                            assert(False)
            if self.last or not repeat:
                break


def _create_trace(lexer, args):
    index = parser.get_argument(lexer, args, 'id', 0)
    name = parser.get_argument(lexer, args, 'name', 'input')
    last = parser.get_argument(lexer, args, 'last', False)
    return Trace(index, name, last)
base.constructors['trace'] = _create_trace
