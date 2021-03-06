import random

from memsim import parser
from memsim.benchmarks import base


class QSort(base.Benchmark):
    """Benchmark to simulate quick sort."""

    def __init__(self, index, seed, size, input_port, output_port):
        base.Benchmark.__init__(self, index)
        self.seed = seed
        self.size = size
        self.input_port = input_port
        self.output_port = output_port
        self.array = [0] * self.size

    def __str__(self):
        result = '(qsort '
        result += '(id ' + str(self.index) + ')'
        result += '(seed ' + str(self.seed) + ')'
        result += '(size ' + str(self.size) + ')'
        if self.input_port >= 0:
            result += '(input_port ' + str(self.input_port) + ')'
        if self.output_port >= 0:
            result += '(output_port ' + str(self.output_port) + ')'
        result += ')'
        return result

    def _sort(self, left, right):
        stack = [(0, self.size - 1)]
        yield self.write(self.size + 0)
        yield self.write(self.size + 1)
        while len(stack) > 0:
            left, right = stack.pop()
            yield self.read(self.size + len(stack) + 0)
            yield self.read(self.size + len(stack) + 1)
            yield self.read(left)
            pivot = self.array[left]
            a, b = left, right
            while True:
                while a <= right:
                    yield self.read(a)
                    if self.array[a] >= pivot:
                        break
                    a += 1
                while b >= left:
                    yield self.read(b)
                    if self.array[b] <= pivot:
                        break
                    b -= 1
                if a > b:
                    break
                self.array[a], self.array[b] = self.array[b], self.array[a]
                yield self.write(a)
                yield self.write(b)
                a += 1
                b -= 1
            if a - 1 > left:
                yield self.write(self.size + len(stack) + 0)
                yield self.write(self.size + len(stack) + 1)
                stack.append((left, a - 1))
            if right > a:
                yield self.write(self.size + len(stack) + 0)
                yield self.write(self.size + len(stack) + 1)
                stack.append((a, right))

    def run(self):
        rand = random.Random(self.seed)
        for i in xrange(self.size):
            self.array[i] = rand.randint(0, 1 << 30)
            yield self.consume(self.input_port)
        for a in self._sort(0, self.size - 1):
            yield a
        for i in xrange(self.size):
            yield self.produce(self.output_port)


def _create_qsort(lexer, args):
    index = parser.get_argument(lexer, args, 'id', 0)
    seed = parser.get_argument(lexer, args, 'seed', 7)
    size = parser.get_argument(lexer, args, 'size', 1024)
    input_port = parser.get_argument(lexer, args, 'input_port', -1)
    output_port = parser.get_argument(lexer, args, 'output_port', -1)
    return QSort(index, seed, size, input_port, output_port)
base.constructors['qsort'] = _create_qsort
