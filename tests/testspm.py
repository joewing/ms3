
import unittest

import distribution
import machine
import mock
import memory
import lex
from memory.spm import SPM, random_spm
import vhdl


class TestSPM(unittest.TestCase):

    def setUp(self):
        self.machine = machine.MachineType()
        self.main = mock.MockMemory()

    def test_spm1(self):
        spm = SPM(self.main, size=1024, access_time=1, cycle_time=1)
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
        s = random_spm(self.machine, self.main, None, 8193)
        self.assertEqual(str(s), "(spm (size 1024)(memory (mock)))")

    def test_permute(self):
        s = SPM(self.main, size=1024, access_time=1, cycle_time=1)
        s.reset(self.machine)
        dist = distribution.Distribution(1)
        result = s.permute(dist, 5)
        self.assertEqual(result, False)
        result = s.permute(dist, 10000)
        self.assertEqual(result, True)

    def test_generate(self):
        s = SPM(self.main, size=1024, access_time=1, cycle_time=1)
        gen = vhdl.VHDLGenerator()
        result = gen.generate(self.machine, s)
        self.assertEqual(self.main.generated, 1)

    def test_parse(self):
        s = "(spm (size 1024)(access_time 3)(cycle_time 4)"
        s += "(memory (ram (latency 100))))"
        l = lex.Lexer(mock.MockFile(s))
        result = memory.parse_memory(l)
        self.assertEqual(str(result), s)

    def test_simplify(self):
        spm = SPM(self.main, size=1024, access_time=1, cycle_time=1)
        simplified = spm.simplify()
        self.assertEqual(spm, simplified)

    def test_cost(self):
        spm = SPM(self.main, size=1024, access_time=1, cycle_time=1)
        spm.reset(self.machine)
        self.assertEqual(spm.get_cost(), 8 * 1024)

    def test_path(self):
        spm = SPM(self.main, size=1024, access_time=1, cycle_time=1)
        spm.reset(self.machine)
        self.assertEqual(spm.get_path_length(), self.machine.addr_bits)
