
from memsim.access import AccessType
from memsim import parser


class Benchmark(object):
    """Base clase for benchmarks.
        A benchmark is a kernel used to generate an address trace.
    """

    def __init__(self, word_size=4):
        self.word_size = word_size
        self.offset = 0
        self.directory = ''

    def read(self, addr):
        """Generate a read."""
        addr *= self.word_size
        addr += self.offset
        return AccessType.READ, addr, self.word_size

    def write(self, addr):
        """Generate a write."""
        addr *= self.word_size
        addr += self.offset
        return AccessType.WRITE, addr, self.word_size

    def idle(self, cycles):
        """Idle for some number of cycles."""
        return AccessType.IDLE, cycles, 0

    def produce(self, port):
        """Produce a value on the specified port."""
        if port >= 0:
            return AccessType.PRODUCE, port, 0
        else:
            return AccessType.IDLE, 0, 0

    def consume(self, port):
        """Consume a value on the specified port."""
        if port >= 0:
            return AccessType.CONSUME, port, 0
        else:
            return AccessType.IDLE, 0, 0

    def reset(self, offset, directory):
        """Prepare the benchmark to be run and set the address offset."""
        self.offset = offset
        self.directory = directory

    def skip(self, n):
        """Skip the next n accesses.  Returns true if successful.
        This is an optimization for the trace benchmark.
        """
        return False

    def run(self):
        """Run the benchmark.
            Note that the results of a benchmark should be deterministic.
            This function should use 'yield' memory accesses.
        """
        assert(False)


constructors = dict()


def parse_benchmark(lexer):
    """Parse a benchmark."""
    return parser.parse(lexer, constructors)
