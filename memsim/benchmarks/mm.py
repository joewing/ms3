from memsim import parser
from memsim.benchmarks import base


class MM(base.Benchmark):
    """Benchmark to simulate matrix-matrix multiplication."""

    def __init__(self, index, size, iterations, input_port, output_port):
        base.Benchmark.__init__(self, index)
        self.size = size
        self.iterations = iterations
        self.input_port = input_port
        self.output_port = output_port

    def __str__(self):
        result = '(mm '
        result += '(id ' + str(self.index) + ')'
        result += '(size ' + str(self.size) + ')'
        result += '(iterations ' + str(self.iterations) + ')'
        if self.input_port >= 0:
            result += '(input_port ' + str(self.input_port) + ')'
        if self.output_port >= 0:
            result += '(output_port ' + str(self.output_port) + ')'
        result += ')'
        return result

    def run(self):
        msize = self.size * self.size
        srca = 0 * msize
        srcb = 1 * msize
        dest = 2 * msize
        for i in xrange(self.iterations):
            if self.input_port >= 0:
                for a in xrange(self.size):
                    for b in xrange(self.size):
                        yield self.consume(self.input_port)
                        yield self.write(srca + a * self.size + b)
                        yield self.write(srcb + a * self.size + b)
            for a in xrange(self.size):
                for b in xrange(self.size):
                    for c in xrange(self.size):
                        yield self.read(srca + b * self.size + c)
                        yield self.read(srcb + c * self.size + a)
                    if self.output_port >= 0:
                        yield self.produce(self.output_port)
                    else:
                        yield self.write(dest + a * self.size + b)


def _create_mm(lexer, args):
    index = parser.get_argument(lexer, args, 'id', 0)
    size = parser.get_argument(lexer, args, 'size', 64)
    iterations = parser.get_argument(lexer, args, 'iterations', 1)
    input_port = parser.get_argument(lexer, args, 'input_port', -1)
    output_port = parser.get_argument(lexer, args, 'output_port', -1)
    return MM(index=index, size=size, iterations=iterations,
              input_port=input_port, output_port=output_port)
base.constructors['mm'] = _create_mm
