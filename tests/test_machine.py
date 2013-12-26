
import unittest

from memsim import lex, machine
from tests import mocks


class TestMachine(unittest.TestCase):

    def test_type(self):
        m = machine.MachineType(machine.TargetType.SIMPLE, 1e9, 4, 32)
        self.assertEqual(m.word_size, 4)
        self.assertEqual(m.word_bits, 2)
        self.assertEqual(m.word_mask, 3)
        self.assertEqual(m.addr_bits, 32)
        self.assertEqual(m.addr_mask, 0xFFFFFFFF)

    def test_parse1(self):
        s = "(target simple)(frequency 1.0)(word_size 2)"
        s += "(addr_bits 16)(max_path 10)(max_cost 9)"
        l = lex.Lexer(mocks.MockFile(s))
        result = machine.parse_machine(l)
        self.assertEqual(str(result), s)
