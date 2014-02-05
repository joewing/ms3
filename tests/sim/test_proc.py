import unittest

from memsim import access, machine, memory, sim
from tests import mocks


class TestProcess(unittest.TestCase):

    def test_step1(self):
        actions = [
            (access.AccessType.READ, 8, 4),
            (access.AccessType.WRITE, 16, 8),
            (access.AccessType.READ, 7, 16),
            (access.AccessType.IDLE, 32, 0),
            (access.AccessType.CONSUME, 1, 0),
            (access.AccessType.PRODUCE, 2, 0),
            (access.AccessType.END, 3, 0)
        ]
        mem = mocks.MockMemory()
        mach = machine.MachineType()
        ml = memory.MemoryList(mem)
        ml.add_memory()
        pl = sim.ProcessList(mach, '.')
        pl.add_benchmark(mocks.MockBenchmark(actions), 0)
        pl.add_fifo(1, sim.fifo.FIFO(128, 8))
        pl.add_fifo(2, sim.fifo.FIFO(256, 8))
        p = pl.processes[0]
        p.reset(mach, mem)
        pl.fifos[1].reset(mach, mem)
        pl.fifos[2].reset(mach, mem)

        t = p.step()    # Read
        self.assertEqual(t, 400)
        self.assertEqual(mem.reads, 1)
        self.assertEqual(mem.writes, 0)
        self.assertEqual(mem.last_addr, 8)
        self.assertEqual(mem.last_size, 4)

        t = p.step()    # Write
        self.assertEqual(t, 800)
        self.assertEqual(mem.reads, 1)
        self.assertEqual(mem.writes, 1)
        self.assertEqual(mem.last_addr, 16)
        self.assertEqual(mem.last_size, 8)

        t = p.step()    # Read
        self.assertEqual(t, 1600)
        self.assertEqual(mem.reads, 4)
        self.assertEqual(mem.writes, 1)
        self.assertEqual(mem.last_addr, 16)
        self.assertEqual(mem.last_size, 7)

        t = p.step()    # Idle
        self.assertEqual(t, 32)
        self.assertEqual(mem.reads, 4)
        self.assertEqual(mem.writes, 1)
        self.assertEqual(mem.last_addr, 16)
        self.assertEqual(mem.last_size, 7)

        t = p.step()    # Consume
        self.assertEqual(t, -1)
        self.assertEqual(mem.reads, 4)
        self.assertEqual(mem.writes, 1)
        self.assertEqual(mem.last_addr, 16)
        self.assertEqual(mem.last_size, 7)
        self.assertEqual(p.consume_waiting, 1)

        pl.produce(1)
        t = p.step()    # Consume produced value.
        self.assertEqual(t, 800)
        self.assertEqual(mem.reads, 5)
        self.assertEqual(mem.writes, 2)
        self.assertEqual(p.consume_waiting, -1)

        t = p.step()    # Produce
        self.assertEqual(t, 800)
        self.assertEqual(mem.reads, 5)
        self.assertEqual(mem.writes, 3)
        self.assertEqual(mem.last_addr, 1024)
        self.assertEqual(mem.last_size, 8)
        self.assertEqual(p.consume_waiting, -1)
        self.assertEqual(p.produce_waiting, -1)

        t = p.step()    # End
        self.assertEqual(t, 0)
        self.assertEqual(mem.reads, 5)
        self.assertEqual(mem.writes, 3)
        self.assertEqual(mem.last_addr, 1024)
        self.assertEqual(mem.last_size, 8)
        self.assertEqual(p.consume_waiting, -1)
        self.assertEqual(p.produce_waiting, -1)
