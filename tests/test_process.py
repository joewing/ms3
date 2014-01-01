
import unittest

from memsim import machine, process
from tests import mocks


class TestProcess(unittest.TestCase):

    def test_step1(self):
        actions = [
            (process.AccessType.READ, 8, 4),
            (process.AccessType.WRITE, 16, 8),
            (process.AccessType.READ, 7, 16),
            (process.AccessType.IDLE, 32, 0),
            (process.AccessType.CONSUME, 1, 0),
            (process.AccessType.PRODUCE, 2, 0),
            (process.AccessType.END, 3, 0)
        ]
        mem = mocks.MockMemory()
        mach = machine.MachineType()
        p = process.Process(mocks.MockBenchmark(actions))
        p.reset(mach, mem, 0, '')

        t = p.step()
        self.assertEqual(t, 400)
        self.assertEqual(mem.reads, 1)
        self.assertEqual(mem.writes, 0)
        self.assertEqual(mem.last_addr, 8)
        self.assertEqual(mem.last_size, 4)

        t = p.step()
        self.assertEqual(t, 800)
        self.assertEqual(mem.reads, 1)
        self.assertEqual(mem.writes, 1)
        self.assertEqual(mem.last_addr, 16)
        self.assertEqual(mem.last_size, 8)

        t = p.step()
        self.assertEqual(t, 1600)
        self.assertEqual(mem.reads, 4)
        self.assertEqual(mem.writes, 1)
        self.assertEqual(mem.last_addr, 16)
        self.assertEqual(mem.last_size, 7)

        t = p.step()
        self.assertEqual(t, 32)
        self.assertEqual(mem.reads, 4)
        self.assertEqual(mem.writes, 1)
        self.assertEqual(mem.last_addr, 16)
        self.assertEqual(mem.last_size, 7)

        t = p.step()
        self.assertEqual(t, 0)
        self.assertEqual(mem.reads, 4)
        self.assertEqual(mem.writes, 1)
        self.assertEqual(mem.last_addr, 16)
        self.assertEqual(mem.last_size, 7)
        self.assertEqual(p.waiting, 1)

        t = p.step()
        self.assertEqual(t, 0)
        self.assertEqual(mem.reads, 4)
        self.assertEqual(mem.writes, 1)
        self.assertEqual(mem.last_addr, 16)
        self.assertEqual(mem.last_size, 7)
        self.assertEqual(p.waiting, 1)

        t = p.step()
        self.assertEqual(t, 0)
        self.assertEqual(mem.reads, 4)
        self.assertEqual(mem.writes, 1)
        self.assertEqual(mem.last_addr, 16)
        self.assertEqual(mem.last_size, 7)
        self.assertEqual(p.waiting, 1)
