
import unittest

from memsim import lex, machine, memory
from memsim.memory import join
from memsim.memory.offset import Offset
from tests import mocks


class TestOffset(unittest.TestCase):

    def setUp(self):
        self.machine = machine.MachineType()
        self.main = mocks.MockMemory()
        self.bank = mocks.MockMemory(join.Join())

    def test_positive(self):
        offset = Offset(self.bank, self.main, 3)
        offset.reset(self.machine)

        t = offset.process(0, False, 0, 8)
        self.assertEqual(t, 1600)
        self.assertEqual(self.bank.reads, 2)
        self.assertEqual(self.bank.writes, 0)
        self.assertEqual(self.main.reads, 2)
        self.assertEqual(self.main.writes, 0)
        self.assertEqual(self.main.last_addr, 5)
        self.assertEqual(self.main.last_size, 3)
        self.assertEqual(self.bank.last_addr, 8)
        self.assertEqual(self.bank.last_size, 3)

        t = offset.process(0, False, 5, 8)
        self.assertEqual(t, 1600)
        self.assertEqual(self.bank.reads, 3)
        self.assertEqual(self.bank.writes, 0)
        self.assertEqual(self.main.reads, 4)
        self.assertEqual(self.main.writes, 0)
        self.assertEqual(self.main.last_addr, 8)
        self.assertEqual(self.main.last_size, 5)
        self.assertEqual(self.bank.last_addr, 8)
        self.assertEqual(self.bank.last_size, 8)

        t = offset.process(0, True, 5, 4)
        self.assertEqual(t, 800)
        self.assertEqual(self.bank.reads, 3)
        self.assertEqual(self.bank.writes, 1)
        self.assertEqual(self.main.reads, 4)
        self.assertEqual(self.main.writes, 2)
        self.assertEqual(self.main.last_addr, 8)
        self.assertEqual(self.main.last_size, 1)
        self.assertEqual(self.bank.last_addr, 8)
        self.assertEqual(self.bank.last_size, 4)

        t = offset.process(0, True, 2, 8)
        self.assertEqual(t, 1600)
        self.assertEqual(self.bank.reads, 3)
        self.assertEqual(self.bank.writes, 3)
        self.assertEqual(self.main.reads, 4)
        self.assertEqual(self.main.writes, 5)
        self.assertEqual(self.main.last_addr, 8)
        self.assertEqual(self.main.last_size, 2)
        self.assertEqual(self.bank.last_addr, 8)
        self.assertEqual(self.bank.last_size, 5)

        t = offset.process(0, False, (1 << 32) - 6, 8)
        self.assertEqual(t, 1600)
        self.assertEqual(self.bank.reads, 5)
        self.assertEqual(self.bank.writes, 3)
        self.assertEqual(self.main.reads, 7)
        self.assertEqual(self.main.writes, 5)
        self.assertEqual(self.main.last_addr, 0)
        self.assertEqual(self.main.last_size, 2)
        self.assertEqual(self.bank.last_addr, 0)
        self.assertEqual(self.bank.last_size, 5)

        t = offset.process(0, False, 7, 16)
        self.assertEqual(t, 3200)
        self.assertEqual(self.bank.reads, 8)
        self.assertEqual(self.bank.writes, 3)
        self.assertEqual(self.main.reads, 12)
        self.assertEqual(self.main.writes, 5)
        self.assertEqual(self.main.last_addr, 21)
        self.assertEqual(self.main.last_size, 2)
        self.assertEqual(self.bank.last_addr, 24)
        self.assertEqual(self.bank.last_size, 2)

    def test_simplify1(self):
        offset = Offset(join.Join(), self.main, 0)
        offset.reset(self.machine)
        simplified = offset.simplify()
        self.assertEqual(str(simplified), '(mock)')

    def test_simplify2(self):
        offset = Offset(self.bank, self.main, 0)
        offset.reset(self.machine)
        simplified = offset.simplify()
        self.assertEqual(str(simplified), '(mock (mock))')

    def test_simplify3(self):
        offset = Offset(self.bank, self.main, 1)
        offset.reset(self.machine)
        simplified = offset.simplify()
        self.assertEqual(str(simplified), '(offset (value 1)' +
                         '(bank (mock (join)))(memory (mock)))')

    def test_simplify4(self):
        offset1 = Offset(self.bank, join.Join(), 1)
        offset2 = Offset(offset1, self.main, 2)
        offset2.reset(self.machine)
        simplified = offset2.simplify()
        s = '(offset (value 3)(bank (mock (join)))(memory (mock)))'
        self.assertEqual(str(simplified), s)

    def test_parse(self):
        s = '(offset (value 8)(bank (join))(memory (ram)))'
        l = lex.Lexer(mocks.MockFile(s))
        result = memory.parse_memory(l)
        expected = '(offset (value 8)(bank (join))(memory (main)))'
        self.assertEqual(str(result), expected)

    def test_path(self):
        offset = Offset(self.bank, self.main, 1)
        offset.reset(self.machine)
        self.assertEqual(offset.get_path_length(), self.machine.addr_bits)

    def test_cost(self):
        offset = Offset(self.bank, self.main, 1)
        self.assertEqual(offset.get_cost(), 0)
