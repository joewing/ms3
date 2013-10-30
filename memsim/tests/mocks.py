
from .. import benchmarks
from .. import memory


class MockFile:

    def __init__(self, value):
        self.value = value
        self.index = 0
        self.name = "mock"

    def read(self, size):
        if self.index + size >= len(self.value):
            size = len(self.value) - self.index + 1
        result = self.value[self.index:self.index + size]
        self.index += size
        return result


class MockBenchmark(benchmarks.Benchmark):

    def __init__(self, seq, word_size=4):
        benchmarks.Benchmark.__init__(self, word_size)
        self.seq = seq

    def run(self):
        for x in self.seq:
            yield x


class MockMemory(memory.Memory):

    last_addr = 0
    last_size = 0
    reads = 0
    writes = 0
    generated = 0

    def __init__(self, mem=None, *banks):
        memory.Memory.__init__(self)
        self.mem = mem
        self.banks = list(banks)

    def __str__(self):
        if self.mem is None:
            return "(mock)"
        result = "(mock " + str(self.mem)
        for c in self.banks:
            result += str(c)
        result += ")"
        return result

    def get_next(self):
        return self.mem

    def set_next(self, n):
        self.mem = n

    def get_banks(self):
        return self.banks

    def set_bank(self, i, b):
        assert(i < len(self.banks))
        self.banks[i] = b

    def generate(self, gen, mach):
        self.generated += 1

    def process(self, start, write, addr, size):
        self.last_addr = addr
        self.last_size = size
        if write:
            self.writes += 1
        else:
            self.reads += 1
        result = start + 100 * size
        if self.mem is not None:
            result = self.mem.process(result, write, addr, size)
        return result
