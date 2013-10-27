
import unittest

import benchmarks
from benchmarks.heap import Heap
import lex
from process import AccessType
import mock


class TestHeap(unittest.TestCase):

    def test_heap(self):
        h = Heap(1, 2, 3, 4)
        h.reset(1024)
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

    def test_parse(self):
        s = "(heap (seed 1)(size 2)(input_port 3)(output_port 4))"
        l = lex.Lexer(mock.MockFile(s))
        result = benchmarks.parse_benchmark(l)
        self.assertEqual(str(result), s)
