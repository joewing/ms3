from abc import ABCMeta, abstractmethod

from memsim.access import AccessType
from memsim import parser


constructors = dict()


class Benchmark(object):
    """Base clase for benchmarks.
        A benchmark is a kernel used to generate an address trace.
    """
    __metaclass__ = ABCMeta

    def __init__(self, index, word_size=4):
        self.index = index
        self.word_size = word_size
        self.directory = ''
        self.max_addr = -1

    def read(self, addr):
        """Generate a read."""
        addr *= self.word_size
        return AccessType.READ, addr, self.word_size

    def write(self, addr):
        """Generate a write."""
        addr *= self.word_size
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

    def reset(self, directory):
        """Prepare the benchmark to be run."""
        self.directory = directory

    def get_size(self, directory):
        """Get the address range of the benchmark in bytes."""
        if self.max_addr < 0:
            self.reset(directory)
            for t, addr, size in self.run():
                if t == AccessType.READ or t == AccessType.WRITE:
                    self.max_addr = max(self.max_addr, addr + size)
        return max(self.max_addr, 0)

    @abstractmethod
    def run(self):
        """Run the benchmark.
            Note that the results of a benchmark should be deterministic.
            This function should use 'yield' memory accesses.
        """
        assert(False)


def parse_benchmark(lexer):
    """Parse a benchmark."""
    return parser.parse(lexer, constructors)
