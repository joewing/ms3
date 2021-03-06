
import unittest

from memsim import benchmarks, lex
from memsim.benchmarks.hash import Hash
from memsim.access import AccessType
from tests import mocks


class TestHash(unittest.TestCase):

    def test_hash(self):
        h = Hash(7, 1, 2, 1, 2)
        h.reset(1024, '')
        gen = h.run()

        t, addr, size = next(gen)
        self.assertEqual(t, AccessType.CONSUME)
        self.assertEqual(addr, 1)

        t, addr, size = next(gen)
        self.assertEqual(t, AccessType.READ)
        self.assertGreaterEqual(addr, 1024)
        self.assertEqual(size, 4)

        t, addr, size = next(gen)
        self.assertEqual(t, AccessType.PRODUCE)
        self.assertEqual(addr, 2)

        t, addr, size = next(gen)
        self.assertEqual(t, AccessType.CONSUME)
        self.assertEqual(addr, 1)

        t, addr, size = next(gen)
        self.assertEqual(t, AccessType.READ)
        self.assertGreaterEqual(addr, 1024)
        self.assertEqual(size, 4)

        t, addr, size = next(gen)
        self.assertEqual(t, AccessType.PRODUCE)
        self.assertEqual(addr, 2)

    def test_parse(self):
        s = "(hash (id 2)(seed 3)(count 10)(input_port 5)(output_port 6))"
        l = lex.Lexer(mocks.MockFile(s))
        result = benchmarks.parse_benchmark(l)
        self.assertEqual(str(result), s)
