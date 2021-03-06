from memsim import benchmarks, memory


class MockRandom(object):

    def __init__(self, sequence):
        self.sequence = sequence
        self.index = 0

    def randint(self, lower, upper):
        result = self.sequence[self.index]
        assert(result >= lower)
        assert(result < upper)
        self.index += 1
        return result


class MockFile(object):

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

    def __init__(self, seq=[], word_size=4):
        benchmarks.Benchmark.__init__(self, 1, word_size)
        self.seq = seq

    def run(self):
        for x in self.seq:
            yield x


class MockMemory(memory.Memory):

    index = 1
    last_addr = 0
    last_size = 0
    reads = 0
    writes = 0
    generated = 0

    def __init__(self, mem=None, *banks):
        memory.Memory.__init__(self)
        self.mem = mem
        self.banks = list(banks)
        self.word_size = 8
        self.main_memory = None

    def __str__(self):
        if self.mem is None:
            return "(mock)"
        result = "(mock " + str(self.mem)
        for c in self.banks:
            result += str(c)
        result += ")"
        return result

    def get_word_size(self):
        return self.word_size

    def can_remove(self):
        return True

    def can_insert(self):
        return True

    def get_name(self):
        return str(self)

    def set_main(self, m):
        self.main_memory = m
        return self

    def get_main(self):
        return self.main_memory

    def get_next(self):
        return self.mem

    def set_next(self, n):
        self.mem = n

    def get_banks(self):
        return self.banks

    def set_bank(self, i, b):
        assert(i < len(self.banks))
        self.banks[i] = b

    def generate(self, gen, source):
        self.generated += 1
        return 'mock'

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
