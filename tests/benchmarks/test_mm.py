
import unittest

from memsim import benchmarks, lex
from memsim.benchmarks.mm import MM
from memsim.access import AccessType
from tests import mocks


class TestMM(unittest.TestCase):

    def test_mm1(self):
        m = MM(2, 1, 3, 4)
        m.reset(1024, '')
        gen = m.run()

        expected = [
            (AccessType.CONSUME, 3, 0),
            (AccessType.WRITE, 1024, 4),
            (AccessType.WRITE, 1040, 4),
            (AccessType.CONSUME, 3, 0),
            (AccessType.WRITE, 1028, 4),
            (AccessType.WRITE, 1044, 4),
            (AccessType.CONSUME, 3, 0),
            (AccessType.WRITE, 1032, 4),
            (AccessType.WRITE, 1048, 4),
            (AccessType.CONSUME, 3, 0),
            (AccessType.WRITE, 1036, 4),
            (AccessType.WRITE, 1052, 4),
            (AccessType.READ, 1024, 4),
            (AccessType.READ, 1040, 4),
            (AccessType.READ, 1028, 4),
            (AccessType.READ, 1048, 4),
            (AccessType.PRODUCE, 4, 0),
            (AccessType.READ, 1032, 4),
            (AccessType.READ, 1040, 4),
            (AccessType.READ, 1036, 4),
            (AccessType.READ, 1048, 4),
            (AccessType.PRODUCE, 4, 0),
            (AccessType.READ, 1024, 4),
            (AccessType.READ, 1044, 4),
            (AccessType.READ, 1028, 4),
            (AccessType.READ, 1052, 4),
            (AccessType.PRODUCE, 4, 0),
            (AccessType.READ, 1032, 4),
            (AccessType.READ, 1044, 4),
            (AccessType.READ, 1036, 4),
            (AccessType.READ, 1052, 4),
            (AccessType.PRODUCE, 4, 0)
        ]
        for e in expected:
            actual = next(gen)
            self.assertEqual(actual, e)

    def test_mm2(self):
        m = MM(1, 1, -1, -1)
        m.reset(1024, '')
        gen = m.run()

        expected = [
            (AccessType.READ, 1024, 4),
            (AccessType.READ, 1028, 4),
            (AccessType.WRITE, 1032, 4)
        ]
        for e in expected:
            actual = next(gen)
            self.assertEqual(actual, e)

    def test_parse(self):
        s = "(mm (size 2)(iterations 1)(input_port 3)(output_port 4))"
        l = lex.Lexer(mocks.MockFile(s))
        result = benchmarks.parse_benchmark(l)
        self.assertEqual(str(result), s)
