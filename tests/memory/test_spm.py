import unittest
import random

from memsim import lex, machine, memory, vhdl
from memsim.sim.memdist import MemoryDistribution
from memsim.memory import MemoryList
from memsim.memory.spm import SPM, random_spm
from memsim.memory.subsystem import Subsystem
from tests import mocks


class TestSPM(unittest.TestCase):

    def setUp(self):
        self.machine = machine.MachineType()
        self.main = mocks.MockMemory()

    def test_spm1(self):
        spm = SPM(self.main, word_size=8, size=1024,
                  access_time=1, cycle_time=1)
        spm.reset(self.machine)

        t = spm.process(0, False, 0, 1)
        self.assertEqual(t, 1)
        self.machine.time += t

        t = spm.process(0, False, 1024 - 8, 8)
        self.assertEqual(t, 1)
        self.machine.time += t

        t = spm.process(0, False, 1024, 4)
        self.assertEqual(t, 400)
        self.machine.time += t

        t = spm.process(0, False, 1023, 2)
        self.assertEqual(t, 101)
        self.machine.time += t

        t = spm.process(0, True, 1024, 1)
        self.assertEqual(t, 100)
        self.machine.time += t

        t = spm.process(0, True, 8192, 16)
        self.assertEqual(t, 1600)
        self.machine.time += t

    def test_random(self):
        dist = MemoryDistribution(random.Random(1))
        s = random_spm(self.machine, self.main, dist, 8193)
        expected = '(spm (word_size 8)(size 1024)(memory (mock)))'
        self.assertEqual(str(s), expected)

    def test_permute(self):
        s = SPM(self.main, word_size=1, size=1,
                access_time=1, cycle_time=1)
        s.reset(self.machine)
        dist = MemoryDistribution(mocks.MockRandom([0, 1]))
        result = s.permute(dist, 1, 0)
        self.assertEqual(result, False)
        result = s.permute(dist, 10000, 0)
        self.assertEqual(result, True)

    def test_generate(self):
        s = SPM(self.main, word_size=8, size=1024,
                access_time=1, cycle_time=1)
        gen = vhdl.VHDLGenerator(self.machine)
        ml = MemoryList(self.main)
        ml.add_memory(Subsystem(0, 8, s))
        result = gen.generate(ml)
        self.assertNotEqual(result, None)
        self.assertEqual(self.main.generated, 1)

    def test_parse(self):
        s = '(spm (word_size 8)(size 1024)(access_time 3)(cycle_time 4)'
        s += '(memory (ram (word_size 4)(latency 100))))'
        l = lex.Lexer(mocks.MockFile(s))
        result = memory.parse_memory(l)
        expected = '(spm (word_size 8)(size 1024)'
        expected += '(access_time 3)(cycle_time 4)'
        expected += '(memory (main)))'
        self.assertEqual(str(result), expected)

    def test_simplify(self):
        spm = SPM(self.main, word_size=4, size=1024,
                  access_time=1, cycle_time=1)
        simplified = spm.simplify()
        self.assertEqual(spm, simplified)

    def test_cost(self):
        spm = SPM(self.main, word_size=8, size=1024,
                  access_time=1, cycle_time=1)
        spm.reset(self.machine)
        self.assertEqual(spm.get_cost(), 8 * 1024)

    def test_path(self):
        spm = SPM(self.main, word_size=2, size=1024,
                  access_time=1, cycle_time=1)
        spm.reset(self.machine)
        self.assertEqual(spm.get_path_length(), 2 * self.machine.addr_bits)
