import unittest

from memsim import lex, machine, memory, vhdl
from memsim.memory import join, MemoryList
from memsim.memory.xor import XOR
from memsim.memory.subsystem import Subsystem
from tests import mocks


class TestXOR(unittest.TestCase):

    def setUp(self):
        self.machine = machine.MachineType()
        self.main = mocks.MockMemory()
        self.join = join.Join()
        self.bank = mocks.MockMemory(self.join)

    def test_xor16(self):
        xor = XOR(self.bank, self.main, 16)
        xor.reset(self.machine)

        t = xor.process(0, False, 32, 8)
        self.assertEqual(t, 1600)
        self.assertEqual(self.bank.reads, 1)
        self.assertEqual(self.bank.writes, 0)
        self.assertEqual(self.main.reads, 1)
        self.assertEqual(self.main.writes, 0)
        self.assertEqual(self.main.last_addr, 32)
        self.assertEqual(self.main.last_size, 8)
        self.assertEqual(self.bank.last_addr, 48)
        self.assertEqual(self.bank.last_size, 8)

    def test_simplify1(self):
        xor = XOR(self.bank, self.main, 16)
        simplified = xor.simplify()
        self.assertEqual(xor, simplified)

    def test_simplify2(self):
        xor = XOR(self.bank, self.main, 0)
        simplified = xor.simplify()
        self.assertEqual(simplified, self.bank)

    def test_simplify3(self):
        xor = XOR(self.join, self.main, 8)
        simplified = xor.simplify()
        self.assertEqual(simplified, self.main)

    def test_simplify4(self):
        xor1 = XOR(self.bank, join.Join(), 8)
        xor2 = XOR(xor1, self.main, 16)
        simplified = xor2.simplify()
        s = '(xor (value 24)(bank (mock (join)))(memory (mock)))'
        self.assertEqual(str(simplified), s)

    def test_simplify5(self):
        xor1 = XOR(self.bank, join.Join(), 8)
        xor2 = XOR(xor1, self.main, 8)
        simplified = xor2.simplify()
        self.assertEqual(str(simplified), "(mock (mock))")

    def test_parse(self):
        to_parse = '(xor (value 1024)(bank (join))(memory (ram)))'
        expected = '(xor (value 1024)(bank (join))(memory (main)))'
        l = lex.Lexer(mocks.MockFile(to_parse))
        result = memory.parse_memory(l)
        self.assertEqual(str(result), expected)

    def test_generate(self):
        xor = XOR(self.bank, self.main, 16)
        gen = vhdl.VHDLGenerator(self.machine)
        ml = MemoryList(self.main)
        ml.add_memory(Subsystem(0, 8, 0, xor))
        result = gen.generate(ml)
        self.assertNotEqual(result, None)
        self.assertEqual(self.main.generated, 1)
        self.assertEqual(self.bank.generated, 1)

    def test_cost(self):
        xor = XOR(self.bank, self.main, 8)
        self.assertEqual(xor.get_cost(), 0)

    def test_path(self):
        xor = XOR(self.bank, self.main, 8)
        self.assertEqual(xor.get_path_length(), 1)
