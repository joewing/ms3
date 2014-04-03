
import unittest

from memsim import lex, machine, memory, vhdl
from memsim.memory import join, MemoryList
from memsim.memory.split import Split
from memsim.memory.subsystem import Subsystem
from tests import mocks


class TestSplit(unittest.TestCase):

    def setUp(self):
        self.machine = machine.MachineType(frequency=1e9,
                                           addr_bits=32)
        self.main = mocks.MockMemory()
        self.join0 = join.Join(0)
        self.join1 = join.Join(1)
        self.bank0 = mocks.MockMemory(self.join0)
        self.bank1 = mocks.MockMemory(self.join1)

    def test_split256(self):
        split = Split(self.bank0, self.bank1,
                      self.main, offset=256)
        split.reset(self.machine)

        t = split.process(0, False, 0, 8)
        self.assertEqual(t, 1600)
        self.assertEqual(self.bank0.reads, 1)
        self.assertEqual(self.bank1.reads, 0)
        self.assertEqual(self.bank0.writes, 0)
        self.assertEqual(self.bank1.writes, 0)
        self.assertEqual(self.main.reads, 1)
        self.assertEqual(self.main.writes, 0)

        t = split.process(0, False, 256, 8)
        self.assertEqual(t, 1600)
        self.assertEqual(self.bank0.reads, 1)
        self.assertEqual(self.bank1.reads, 1)
        self.assertEqual(self.bank0.writes, 0)
        self.assertEqual(self.bank1.writes, 0)
        self.assertEqual(self.main.reads, 2)
        self.assertEqual(self.main.writes, 0)

        t = split.process(0, True, 256, 8)
        self.assertEqual(t, 1600)
        self.assertEqual(self.bank0.reads, 1)
        self.assertEqual(self.bank1.reads, 1)
        self.assertEqual(self.bank0.writes, 0)
        self.assertEqual(self.bank1.writes, 1)
        self.assertEqual(self.main.reads, 2)
        self.assertEqual(self.main.writes, 1)

        t = split.process(0, True, 0, 8)
        self.assertEqual(t, 1600)
        self.assertEqual(self.bank0.reads, 1)
        self.assertEqual(self.bank1.reads, 1)
        self.assertEqual(self.bank0.writes, 1)
        self.assertEqual(self.bank1.writes, 1)
        self.assertEqual(self.main.reads, 2)
        self.assertEqual(self.main.writes, 2)

        t = split.process(0, False, 252, 8)
        self.assertEqual(t, 1600)
        self.assertEqual(self.bank0.reads, 2)
        self.assertEqual(self.bank1.reads, 2)
        self.assertEqual(self.bank0.writes, 1)
        self.assertEqual(self.bank1.writes, 1)
        self.assertEqual(self.main.reads, 4)
        self.assertEqual(self.main.writes, 2)

        t = split.process(0, False, 2 ** 32 - 4, 8)
        self.assertEqual(t, 1600)
        self.assertEqual(self.bank0.reads, 3)
        self.assertEqual(self.bank1.reads, 3)
        self.assertEqual(self.bank0.writes, 1)
        self.assertEqual(self.bank1.writes, 1)
        self.assertEqual(self.main.reads, 6)
        self.assertEqual(self.main.writes, 2)

        t = split.process(0, False, 2 ** 32 - 1, 1)
        self.assertEqual(t, 200)
        self.assertEqual(self.bank0.reads, 3)
        self.assertEqual(self.bank1.reads, 4)
        self.assertEqual(self.bank0.writes, 1)
        self.assertEqual(self.bank1.writes, 1)
        self.assertEqual(self.main.reads, 7)
        self.assertEqual(self.main.writes, 2)

        t = split.process(0, False, 255, 1)
        self.assertEqual(t, 200)
        self.assertEqual(self.bank0.reads, 4)
        self.assertEqual(self.bank1.reads, 4)
        self.assertEqual(self.bank0.writes, 1)
        self.assertEqual(self.bank1.writes, 1)
        self.assertEqual(self.main.reads, 8)
        self.assertEqual(self.main.writes, 2)

        t = split.process(0, False, 2 ** 32 - 1, 2)
        self.assertEqual(t, 400)
        self.assertEqual(self.bank0.reads, 5)
        self.assertEqual(self.bank1.reads, 5)
        self.assertEqual(self.bank0.writes, 1)
        self.assertEqual(self.bank1.writes, 1)
        self.assertEqual(self.main.reads, 10)
        self.assertEqual(self.main.writes, 2)

    def test_simplify1(self):
        split = Split(self.bank0, self.bank1, self.main, offset=256)
        simplified = split.simplify()
        self.assertEqual(simplified, split)

    def test_simplify2(self):
        split = Split(self.bank0, self.bank1, self.main, offset=0)
        simplified = split.simplify()
        self.assertEqual(simplified, split)

    def test_simplify3(self):
        split = Split(self.join0, self.join1, self.main, offset=256)
        simplified = split.simplify()
        self.assertEqual(simplified, self.main)

    def test_parse(self):
        s = '(split (offset 128)(bank0 (join))(bank1 (join))'
        s += '(memory (ram)))'
        l = lex.Lexer(mocks.MockFile(s))
        result = memory.parse_memory(l)
        expected = '(split (offset 128)(bank0 (join))(bank1 (join))'
        expected += '(memory (main)))'
        self.assertEqual(str(result), expected)

    def test_generate(self):
        split = Split(self.bank0, self.bank1, self.main, offset=128)
        gen = vhdl.VHDLGenerator(self.machine)
        ml = MemoryList(self.main)
        ml.add_memory(Subsystem(0, 8, 0, split))
        result = gen.generate(ml)
        self.assertNotEqual(result, None)
        self.assertEqual(self.main.generated, 1)
        self.assertEqual(self.bank0.generated, 1)
        self.assertEqual(self.bank1.generated, 1)

    def test_cost(self):
        split = Split(self.bank0, self.bank1, self.main, offset=8)
        self.assertEqual(split.get_cost(), 0)

    def test_path(self):
        split = Split(self.bank0, self.bank1, self.main, offset=8)
        split.reset(self.machine)
        self.assertEqual(split.get_path_length(), self.machine.addr_bits)
