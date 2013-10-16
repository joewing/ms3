
import unittest
import machine
import mock
import process

class TestProcess(unittest.TestCase):

   def test_step1(self):
      actions = [
         (process.AccessType.READ, 8, 4),
         (process.AccessType.WRITE, 16, 8),
         (process.AccessType.IDLE, 32, 0),
         (process.AccessType.CONSUME, 1, 0),
         (process.AccessType.PRODUCE, 2, 0),
         (process.AccessType.END, 3, 0)
      ]
      mem = mock.MockMemory()
      mach = machine.MachineType()
      p = process.Process(None, mock.MockBenchmark(actions))
      p.reset(mach, mem, 0)

      t = p.step(False)
      self.assertEqual(t, 400)
      self.assertEqual(mem.reads, 1)
      self.assertEqual(mem.writes, 0)
      self.assertEqual(mem.last_addr, 8)
      self.assertEqual(mem.last_size, 4)

      t = p.step(False)
      self.assertEqual(t, 800)
      self.assertEqual(mem.reads, 1)
      self.assertEqual(mem.writes, 1)
      self.assertEqual(mem.last_addr, 16)
      self.assertEqual(mem.last_size, 8)

      t = p.step(False)
      self.assertEqual(t, 32)
      self.assertEqual(mem.reads, 1)
      self.assertEqual(mem.writes, 1)
      self.assertEqual(mem.last_addr, 16)
      self.assertEqual(mem.last_size, 8)

      t = p.step(False)
      self.assertEqual(t, 0)
      self.assertEqual(mem.reads, 1)
      self.assertEqual(mem.writes, 1)
      self.assertEqual(mem.last_addr, 16)
      self.assertEqual(mem.last_size, 8)
      self.assertEqual(p.waiting, 1)

      t = p.step(False)
      self.assertEqual(t, 0)
      self.assertEqual(mem.reads, 1)
      self.assertEqual(mem.writes, 1)
      self.assertEqual(mem.last_addr, 16)
      self.assertEqual(mem.last_size, 8)
      self.assertEqual(p.waiting, 1)

      t = p.step(False)
      self.assertEqual(t, 0)
      self.assertEqual(mem.reads, 1)
      self.assertEqual(mem.writes, 1)
      self.assertEqual(mem.last_addr, 16)
      self.assertEqual(mem.last_size, 8)
      self.assertEqual(p.waiting, 1)

