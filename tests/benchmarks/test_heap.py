
import unittest

from memsim import benchmarks, lex
from memsim.benchmarks.heap import Heap
from memsim.access import AccessType
from tests import mocks


class TestHeap(unittest.TestCase):

    def test_heap1(self):
        h = Heap(0, 1, 2, 3, 4)
        h.reset(1024, '')
        gen = h.run()

        # Initialize
        t, addr, size = next(gen)
        self.assertEqual(t, AccessType.WRITE)
        self.assertEqual(addr, 1024)
        self.assertEqual(size, 4)

        # Insert 1
        t, addr, size = next(gen)
        self.assertEqual(t, AccessType.CONSUME)
        self.assertEqual(addr, 3)
        self.assertEqual(size, 0)

        t, addr, size = next(gen)
        self.assertEqual(t, AccessType.READ)
        self.assertEqual(addr, 1024)
        self.assertEqual(size, 4)

        t, addr, size = next(gen)
        self.assertEqual(t, AccessType.WRITE)
        self.assertEqual(addr, 1024)
        self.assertEqual(size, 4)

        t, addr, size = next(gen)
        self.assertEqual(t, AccessType.WRITE)
        self.assertEqual(addr, 1028)
        self.assertEqual(size, 4)

        # Insert 2
        t, addr, size = next(gen)
        self.assertEqual(t, AccessType.CONSUME)
        self.assertEqual(addr, 3)
        self.assertEqual(size, 0)

        t, addr, size = next(gen)
        self.assertEqual(t, AccessType.READ)
        self.assertEqual(addr, 1024)
        self.assertEqual(size, 4)

        t, addr, size = next(gen)
        self.assertEqual(t, AccessType.WRITE)
        self.assertEqual(addr, 1024)
        self.assertEqual(size, 4)

        t, addr, size = next(gen)
        self.assertEqual(t, AccessType.READ)
        self.assertEqual(addr, 1028)
        self.assertEqual(size, 4)

        t, addr, size = next(gen)
        self.assertEqual(t, AccessType.WRITE)
        self.assertEqual(addr, 1032)
        self.assertEqual(size, 4)

        # Remove 1
        t, addr, size = next(gen)
        self.assertEqual(t, AccessType.READ)
        self.assertEqual(addr, 1024)
        self.assertEqual(size, 4)

        t, addr, size = next(gen)
        self.assertEqual(t, AccessType.WRITE)
        self.assertEqual(addr, 1024)
        self.assertEqual(size, 4)

        t, addr, size = next(gen)
        self.assertEqual(t, AccessType.READ)
        self.assertEqual(addr, 1032)
        self.assertEqual(size, 4)

        t, addr, size = next(gen)
        self.assertEqual(t, AccessType.READ)
        self.assertEqual(addr, 1028)
        self.assertEqual(size, 4)

        t, addr, size = next(gen)
        self.assertEqual(t, AccessType.WRITE)
        self.assertEqual(addr, 1028)
        self.assertEqual(size, 4)

        t, addr, size = next(gen)
        self.assertEqual(t, AccessType.PRODUCE)
        self.assertEqual(addr, 4)
        self.assertEqual(size, 0)

        # Remove 2
        t, addr, size = next(gen)
        self.assertEqual(t, AccessType.READ)
        self.assertEqual(addr, 1024)
        self.assertEqual(size, 4)

        t, addr, size = next(gen)
        self.assertEqual(t, AccessType.WRITE)
        self.assertEqual(addr, 1024)
        self.assertEqual(size, 4)

        t, addr, size = next(gen)
        self.assertEqual(t, AccessType.READ)
        self.assertEqual(addr, 1028)
        self.assertEqual(size, 4)

        t, addr, size = next(gen)
        self.assertEqual(t, AccessType.READ)
        self.assertEqual(addr, 1028)
        self.assertEqual(size, 4)

        t, addr, size = next(gen)
        self.assertEqual(t, AccessType.WRITE)
        self.assertEqual(addr, 1028)
        self.assertEqual(size, 4)

        t, addr, size = next(gen)
        self.assertEqual(t, AccessType.PRODUCE)
        self.assertEqual(addr, 4)
        self.assertEqual(size, 0)

    def test_heap2(self):
        h = Heap(1, 1, 1, -1, -1)
        h.reset(1024, '')
        gen = h.run()

        expected = [
            (AccessType.WRITE, 1024, 4),    # initialize
            (AccessType.IDLE, 0, 0),        # 'consume'
            (AccessType.READ, 1024, 4),     # read size
            (AccessType.WRITE, 1024, 4),    # increment size
            (AccessType.WRITE, 1028, 4),    # write first
            (AccessType.READ, 1024, 4),     # read size
            (AccessType.WRITE, 1024, 4),    # decrement size
            (AccessType.READ, 1028, 4),     # read result
            (AccessType.READ, 1028, 4),     # read for over-write
            (AccessType.WRITE, 1028, 4),    # overwrite
            (AccessType.IDLE, 0, 0),        # 'produce'
        ]
        for e in expected:
            actual = next(gen)
            self.assertEqual(actual, e)

    def test_parse(self):
        s = "(heap (id 1)(seed 1)(size 2)(input_port 3)(output_port 4))"
        l = lex.Lexer(mocks.MockFile(s))
        result = benchmarks.parse_benchmark(l)
        self.assertEqual(str(result), s)
