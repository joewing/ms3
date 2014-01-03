
import unittest

from memsim import benchmarks, lex
from memsim.benchmarks.qsort import QSort
from memsim.access import AccessType
from tests import mocks


class TestQSort(unittest.TestCase):

    def test_qsort1(self):
        q = QSort(7, 2, 3, 4)
        q.reset(1024, '')
        gen = q.run()

        expected = [
            (AccessType.CONSUME, 3, 0),
            (AccessType.CONSUME, 3, 0),
            (AccessType.WRITE, 1032, 4),    # push left
            (AccessType.WRITE, 1036, 4),    # push right
            (AccessType.READ, 1032, 4),     # pop left
            (AccessType.READ, 1036, 4),     # pop right
            (AccessType.READ, 1024, 4),     # array[0]  (pivot)
            (AccessType.READ, 1024, 4),     # array[0]  (first loop)
            (AccessType.READ, 1028, 4),     # array[1]  (second loop)
            (AccessType.WRITE, 1024, 4),    # array[0]  (swap A)
            (AccessType.WRITE, 1028, 4),    # array[1]  (swap B)
            (AccessType.READ, 1028, 4),     # array[1]  (first loop)
            (AccessType.READ, 1024, 4),     # array[0]  (second loop)
            (AccessType.PRODUCE, 4, 0),
            (AccessType.PRODUCE, 4, 0),
        ]
        for e in expected:
            actual = next(gen)
            self.assertEqual(actual, e)

    def test_qsort2(self):
        q = QSort(3, 64, -1, -1)
        q.reset(1024, '')
        for _ in q.run():
            pass
        self.assertEqual(sorted(q.array), q.array)

    def test_parse(self):
        s = "(qsort (seed 3)(size 16)(input_port 1)(output_port 2))"
        l = lex.Lexer(mocks.MockFile(s))
        result = benchmarks.parse_benchmark(l)
        self.assertEqual(str(result), s)
