
import unittest

from memsim import lex, memory
from memsim.machine import MachineType
from memsim.memory.dram import DRAM
from tests import mocks


class TestDRAM(unittest.TestCase):

    def setUp(self):
        self.machine = MachineType()
        self.machine.reset()

    def test_open(self):
        dram = DRAM(frequency=1e9 / 2,
                    cas_cycles=2,
                    rcd_cycles=3,
                    rp_cycles=4,
                    wb_cycles=1,
                    page_size=1024,
                    page_count=2048,
                    width=2,
                    burst_size=2,
                    open_page=True,
                    ddr=False)
        dram.reset(self.machine)

        self.assertEqual(dram.get_path_length(), 0)
        self.assertEqual(dram.get_cost(), 0)

        t = dram.process(0, False, 0, 4)  # Miss
        self.assertEqual(t, 22)
        self.machine.time += t

        t = dram.process(0, False, 4, 4)  # Hit
        self.assertEqual(t, 8)
        self.machine.time += t

        t = dram.process(0, False, 2097152, 4)  # Miss
        self.assertEqual(t, 22)
        self.machine.time += t

        t = dram.process(0, True, 2097152, 8)    # Hit
        self.assertEqual(t, 16)
        self.machine.time += t

        t = dram.process(0, True, 2097152 - 4, 8)  # Miss/hit
        self.assertEqual(t, 30)
        self.machine.time += t

        t = dram.process(0, False, 4, 4)  # Miss/write-back
        self.assertEqual(t, 24)
        self.machine.time += t

    def test_closed(self):
        dram = DRAM(frequency=1e9 / 2,
                    cas_cycles=2,
                    rcd_cycles=3,
                    rp_cycles=4,
                    wb_cycles=1,
                    page_size=1024,
                    page_count=2048,
                    width=2,
                    burst_size=2,
                    open_page=False,
                    ddr=False)
        dram.reset(self.machine)

        t = dram.process(0, False, 0, 4)
        self.assertEqual(t, 14)
        self.machine.time += t

        t = dram.process(0, False, 4, 4)
        self.assertEqual(t, 22)
        self.machine.time += t

        t = dram.process(0, False, 2097152, 4)
        self.assertEqual(t, 14)
        self.machine.time += t

        t = dram.process(0, True, 2097152, 8)
        self.assertEqual(t, 46)
        self.machine.time += t

        t = dram.process(0, True, 2097152 - 4, 8)
        self.assertEqual(t, 28)
        self.machine.time += t

        t = dram.process(0, False, 4, 4)
        self.assertEqual(t, 14)
        self.machine.time += t

    def test_simplify(self):
        dram = DRAM(frequency=1e9 / 2,
                    cas_cycles=2,
                    rcd_cycles=3,
                    rp_cycles=4,
                    wb_cycles=1,
                    page_size=1024,
                    page_count=2048,
                    width=2,
                    burst_size=2,
                    open_page=False,
                    ddr=False)
        simplified = dram.simplify()
        self.assertEqual(dram, simplified)

    def test_parse(self):
        s = "(dram (frequency 1024)(cas_cycles 1)(rcd_cycles 2)"
        s += "(rp_cycles 3)(wb_cycles 4)(page_size 8)(page_count 16)"
        s += "(width 2)(burst_size 2)(open_page false)(ddr false))"
        l = lex.Lexer(mocks.MockFile(s))
        result = memory.parse_memory(l)
        self.assertEqual(str(result), s)
