
from memsim import parser
from memsim.benchmarks import base


class Cholesky(base.Benchmark):
    """Benchmark to simulate Cholesky factorization."""

    def __init__(self, size, input_port, output_port):
        base.Benchmark.__init__(self)
        self.size = size
        self.input_port = input_port
        self.output_port = output_port

    def run(self):
        for i in xrange(self.size * self.size):
            yield self.consume(self.input_port)
        for i in xrange(self.size):
            yield self.read(i * self.size + i)
            for j in xrange(i):
                yield self.read(i * self.size + j)
            for j in xrange(i + 1, self.size - 1):
                yield self.read(i * self.size + j)
                for k in xrange(i):
                    yield self.read(j * self.size + k)
                    yield self.read(i * self.size + k)
                yield self.write(j * self.size + i)
                yield self.produce(self.output_port)


def _create_cholesky(lexer, args):
    size = parser.get_argument(lexer, args, 'size', 128)
    input_port = parser.get_argument(lexer, args, 'input_port', -1)
    output_port = parser.get_argument(lexer, args, 'output_port', -1)
    return Cholesky(size, input_port, output_port)
base.constructors['cholesky'] = _create_cholesky
