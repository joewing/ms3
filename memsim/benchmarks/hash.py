
import random

from memsim.benchmarks import base
from memsim import parser


class Hash(base.Benchmark):
    """Benchmark to simulate probes into a hash table."""

    def __init__(self, index, seed, count, input_port, output_port):
        base.Benchmark.__init__(self, index)
        self.seed = seed
        self.count = count
        self.input_port = input_port
        self.output_port = output_port

    def __str__(self):
        result = '(hash '
        result += '(id ' + str(self.index) + ')'
        result += '(seed ' + str(self.seed) + ')'
        result += '(count ' + str(self.count) + ')'
        if self.input_port >= 0:
            result += '(input_port ' + str(self.input_port) + ')'
        if self.output_port >= 0:
            result += '(output_port ' + str(self.output_port) + ')'
        result += ')'
        return result

    def run(self):
        rand = random.Random(self.seed)
        for i in xrange(self.count):
            if self.input_port >= 0:
                yield self.consume(self.input_port)
            addr = rand.randint(0, self.count - 1)
            yield self.read(addr)
            if self.output_port >= 0:
                yield self.produce(self.output_port)


def _create_hash(lexer, args):
    index = parser.get_argument(lexer, args, 'id', 0)
    seed = parser.get_argument(lexer, args, 'seed', 7)
    count = parser.get_argument(lexer, args, 'count', 65536)
    input_port = parser.get_argument(lexer, args, 'input_port', -1)
    output_port = parser.get_argument(lexer, args, 'output_port', -1)
    return Hash(index, seed, count, input_port, output_port)
base.constructors['hash'] = _create_hash
