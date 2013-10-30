
import unittest

from ..memory import join
from .. import lex
from .. import machine
from .. import memory
from ..memory.shift import Shift
from . import mocks


class TestShift(unittest.TestCase):

    def setUp(self):
        self.machine = machine.MachineType()
        self.main = mocks.MockMemory()
        self.join = join.Join()
        self.bank = mocks.MockMemory(self.join)

    def test_positive(self):
        shift = Shift(self.bank, self.main, 1)
        shift.reset(self.machine)

        t = shift.process(0, False, 0, 8)
        self.assertEqual(t, 1600)
        self.assertEqual(self.bank.reads, 1)
        self.assertEqual(self.bank.writes, 0)
        self.assertEqual(self.main.reads, 1)
        self.assertEqual(self.main.writes, 0)
        self.assertEqual(self.main.last_addr, 0)
        self.assertEqual(self.main.last_size, 8)
        self.assertEqual(self.bank.last_addr, 0)
        self.assertEqual(self.bank.last_size, 8)

        t = shift.process(0, False, 1, 1)
        self.assertEqual(t, 200)
        self.assertEqual(self.bank.reads, 2)
        self.assertEqual(self.bank.writes, 0)
        self.assertEqual(self.main.reads, 2)
        self.assertEqual(self.main.writes, 0)
        self.assertEqual(self.main.last_addr, 1)
        self.assertEqual(self.main.last_size, 1)
        self.assertEqual(self.bank.last_addr, 1)
        self.assertEqual(self.bank.last_size, 1)

        t = shift.process(0, True, 1, 1)
        self.assertEqual(t, 200)
        self.assertEqual(self.bank.reads, 2)
        self.assertEqual(self.bank.writes, 1)
        self.assertEqual(self.main.reads, 2)
        self.assertEqual(self.main.writes, 1)
        self.assertEqual(self.main.last_addr, 1)
        self.assertEqual(self.main.last_size, 1)
        self.assertEqual(self.bank.last_addr, 1)
        self.assertEqual(self.bank.last_size, 1)

        t = shift.process(0, False, 16, 8)
        self.assertEqual(t, 1600)
        self.assertEqual(self.bank.reads, 3)
        self.assertEqual(self.bank.writes, 1)
        self.assertEqual(self.main.reads, 3)
        self.assertEqual(self.main.writes, 1)
        self.assertEqual(self.main.last_addr, 16)
        self.assertEqual(self.main.last_size, 8)
        self.assertEqual(self.bank.last_addr, 32)
        self.assertEqual(self.bank.last_size, 8)

        t = shift.process(0, False, 1 << 31, 4)
        self.assertEqual(t, 800)
        self.assertEqual(self.bank.reads, 4)
        self.assertEqual(self.bank.writes, 1)
        self.assertEqual(self.main.reads, 4)
        self.assertEqual(self.main.writes, 1)
        self.assertEqual(self.main.last_addr, 1 << 31)
        self.assertEqual(self.main.last_size, 4)
        self.assertEqual(self.bank.last_addr, 8)
        self.assertEqual(self.bank.last_size, 4)

        t = shift.process(0, False, 105, 2)
        self.assertEqual(t, 400)
        self.assertEqual(self.bank.reads, 5)
        self.assertEqual(self.bank.writes, 1)
        self.assertEqual(self.main.reads, 5)
        self.assertEqual(self.main.writes, 1)
        self.assertEqual(self.main.last_addr, 105)
        self.assertEqual(self.main.last_size, 2)
        self.assertEqual(self.bank.last_addr, 209)
        self.assertEqual(self.bank.last_size, 2)

    def test_negative(self):
        shift = Shift(self.bank, self.main, -2)
        shift.reset(self.machine)

        t = shift.process(0, False, 0, 8)
        self.assertEqual(t, 1600)
        self.assertEqual(self.bank.reads, 1)
        self.assertEqual(self.bank.writes, 0)
        self.assertEqual(self.main.reads, 1)
        self.assertEqual(self.main.writes, 0)
        self.assertEqual(self.main.last_addr, 0)
        self.assertEqual(self.main.last_size, 8)
        self.assertEqual(self.bank.last_addr, 0)
        self.assertEqual(self.bank.last_size, 8)

        t = shift.process(0, False, 1, 1)
        self.assertEqual(t, 200)
        self.assertEqual(self.bank.reads, 2)
        self.assertEqual(self.bank.writes, 0)
        self.assertEqual(self.main.reads, 2)
        self.assertEqual(self.main.writes, 0)
        self.assertEqual(self.main.last_addr, 1)
        self.assertEqual(self.main.last_size, 1)
        self.assertEqual(self.bank.last_addr, 1)
        self.assertEqual(self.bank.last_size, 1)

        t = shift.process(0, True, 1, 1)
        self.assertEqual(t, 200)
        self.assertEqual(self.bank.reads, 2)
        self.assertEqual(self.bank.writes, 1)
        self.assertEqual(self.main.reads, 2)
        self.assertEqual(self.main.writes, 1)
        self.assertEqual(self.main.last_addr, 1)
        self.assertEqual(self.main.last_size, 1)
        self.assertEqual(self.bank.last_addr, 1)
        self.assertEqual(self.bank.last_size, 1)

        t = shift.process(0, True, 9, 4)
        self.assertEqual(t, 800)
        self.assertEqual(self.bank.reads, 2)
        self.assertEqual(self.bank.writes, 2)
        self.assertEqual(self.main.reads, 2)
        self.assertEqual(self.main.writes, 2)
        self.assertEqual(self.main.last_addr, 9)
        self.assertEqual(self.main.last_size, 4)
        self.assertEqual(self.bank.last_addr, (1 << 30) | 1)
        self.assertEqual(self.bank.last_size, 4)

    def test_simplify1(self):
        shift = Shift(self.bank, self.main, 1)
        simplified = shift.simplify()
        self.assertEqual(simplified, shift)

    def test_simplify2(self):
        shift = Shift(self.bank, self.main, 0)
        simplified = shift.simplify()
        self.assertEqual(simplified, self.bank)

    def test_simplify3(self):
        shift = Shift(self.join, self.main, 1)
        simplified = shift.simplify()
        self.assertEqual(simplified, self.main)

    def test_simplify4(self):
        shift1 = Shift(self.bank, join.Join(), 1)
        shift2 = Shift(shift1, self.main, 2)
        simplified = shift2.simplify()
        s = "(shift (value 3)(bank (mock (join)))(memory (mock)))"
        self.assertEqual(str(simplified), s)

    def test_simplify5(self):
        shift1 = Shift(self.bank, join.Join(), -10)
        shift2 = Shift(shift1, self.main, -25)
        simplified = shift2.simplify()
        simplified.reset(self.machine)
        s = "(shift (value 23)(bank (mock (join)))(memory (mock)))"
        self.assertEqual(str(simplified), s)

    def test_parse(self):
        s = "(shift (value 2)(bank (join))(memory (ram (latency 100))))"
        l = lex.Lexer(mocks.MockFile(s))
        result = memory.parse_memory(l)
        self.assertEqual(str(result), s)

    def test_cost(self):
        shift = Shift(self.bank, self.main, 5)
        self.assertEqual(shift.get_cost(), 0)

    def test_path(self):
        shift = Shift(self.bank, self.main, 5)
        self.assertEqual(shift.get_path_length(), 0)
