
import unittest

from memsim import lex, machine
from tests import mocks


class TestMachine(unittest.TestCase):

    def test_type(self):
        m = machine.MachineType(machine.TargetType.SIMPLE, 1e9, 32)
        self.assertEqual(m.addr_bits, 32)
        self.assertEqual(m.addr_mask, 0xFFFFFFFF)

    def test_parse1(self):
        s = "(target simple)(frequency 1.0)"
        s += "(addr_bits 16)(max_path 10)(max_cost 9)"
        l = lex.Lexer(mocks.MockFile(s))
        result = machine.parse_machine(l)
        self.assertEqual(str(result), s)
