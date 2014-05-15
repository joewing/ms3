from memsim import parser
from memsim.access import AccessType
from memsim.benchmarks import base

class Split(base.Benchmark):
    """Benchmark to split a stream."""

    def __init__(self, index, in_port, out0, out1):
        base.Benchmark.__init__(self, index)
        self.in_port = in_port
        self.out0 = out0
        self.out1 = out1

    def __str__(self):
        result = '(split '
        result += '(id ' + str(self.index) + ')'
        result += '(in ' + str(self.in_port) + ')'
        result += '(out0 ' + str(self.out0) + ')'
        result += '(out1 ' + str(self.out1) + ')'
        result += ')'
        return result

    def run(self):
        while True:
            yield AccessType.CONSUME, self.in_port, 0
            yield AccessType.OUTPUT, self.out0, self.out1
            yield AccessType.CONSUME, self.in_port, 0
            yield AccessType.OUTPUT, self.out1, self.out0


def _create_split(lexer, args):
    index = parser.get_argument(lexer, args, 'id', 0)
    in_port = parser.get_argument(lexer, args, 'in', 0)
    out0 = parser.get_argument(lexer, args, 'out0', 0)
    out1 = parser.get_argument(lexer, args, 'out1', 0)
    return Split(index, in_port, out0, out1)
base.constructors['split'] = _create_split
