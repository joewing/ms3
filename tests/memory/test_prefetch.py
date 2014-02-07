
import unittest

from memsim import lex, machine, memory
from memsim.memory.prefetch import Prefetch
from tests import mocks


class TestPrefetch(unittest.TestCase):

    def setUp(self):
        self.machine = machine.MachineType()
        self.machine.reset()
        self.main = mocks.MockMemory()

    def test_positive(self):
        pf = Prefetch(self.main, 8)
        pf.reset(self.machine)

        t = pf.process(0, False, 0, 1)
        self.machine.time += t
        self.assertEqual(t, 100)
        self.assertEqual(self.main.last_addr, 8)
        self.assertEqual(self.main.last_size, 1)

        t = pf.process(0, False, 0, 1)
        self.machine.time += t
        self.assertEqual(t, 200)
        self.assertEqual(self.main.last_addr, 8)
        self.assertEqual(self.main.last_size, 1)

        t = pf.process(0, True, 16, 4)
        self.machine.time += t
        self.assertEqual(t, 500)
        self.assertEqual(self.main.last_addr, 16)
        self.assertEqual(self.main.last_size, 4)

    def test_simplify1(self):
        pf = Prefetch(self.main, 8)
        simplified = pf.simplify()
        self.assertEqual(pf, simplified)

    def test_simplify2(self):
        pf = Prefetch(self.main, 0)
        simplified = pf.simplify()
        self.assertEqual(self.main, simplified)

    def test_parse(self):
        s = '(prefetch (stride -8)(memory (main)))'
        l = lex.Lexer(mocks.MockFile(s))
        result = memory.parse_memory(l)
        self.assertEqual(str(result), s)

    def test_cost(self):
        pf = Prefetch(self.main, 0)
        self.assertEqual(pf.get_cost(), 0)

    def test_path(self):
        pf = Prefetch(self.main, 0)
        self.assertEqual(pf.get_path_length(), 0)
