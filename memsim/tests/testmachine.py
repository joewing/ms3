

import unittest

from memsim import lex, machine
from memsim.tests import mocks


class TestMachine(unittest.TestCase):

    def test_type(self):
        m = machine.MachineType(machine.TargetType.SIMPLE, 1e9, 4, 32)
        self.assertEqual(m.word_size, 4)
        self.assertEqual(m.word_bits, 2)
        self.assertEqual(m.word_mask, 3)
        self.assertEqual(m.addr_bits, 32)
        self.assertEqual(m.addr_mask, 0xFFFFFFFF)

    def test_log2(self):
        self.assertEqual(machine.log2(0), 0)
        self.assertEqual(machine.log2(5), 3)
        self.assertEqual(machine.log2(7), 3)
        self.assertEqual(machine.log2(8), 4)

    def test_rp2(self):
        self.assertEqual(machine.round_power2(4), 4)
        self.assertEqual(machine.round_power2(5), 8)
        self.assertEqual(machine.round_power2(7), 8)
        self.assertEqual(machine.round_power2(8), 8)

    def test_parse1(self):
        s = "(target simple)(frequency 1.0)(word_size 2)"
        s += "(addr_bits 16)(max_path 10)(max_cost 9)"
        l = lex.Lexer(mocks.MockFile(s))
        result = machine.parse_machine(l)
        expected = "(machine " + s + ")"
        self.assertEqual(str(result), expected)
