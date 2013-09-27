

import unittest
from Machine import MachineType
from Memory.Join import Join
from Memory.Offset import Offset
from MockMemory import MockMemory

class TestOffset(unittest.TestCase):

   def setUp(self):
      self.machine = MachineType(word_size = 8, addr_bits = 32)
      self.main = MockMemory()
      self.join = Join(self.machine)
      self.bank = MockMemory(self.join)

   def test_positive(self):
      offset = Offset(self.machine, self.bank, self.main, 3)
      self.join.parent = offset

      t = offset.process(False, 0, 8)
      self.assertEqual(t, 1600)
      self.assertEqual(self.bank.reads, 1)
      self.assertEqual(self.bank.writes, 0)
      self.assertEqual(self.main.reads, 1)
      self.assertEqual(self.main.writes, 0)
      self.assertEqual(self.main.last_addr, 0)
      self.assertEqual(self.main.last_size, 8)
      self.assertEqual(self.bank.last_addr, 3)
      self.assertEqual(self.bank.last_size, 8)

      t = offset.process(False, 5, 8)
      self.assertEqual(t, 1600)
      self.assertEqual(self.bank.reads, 2)
      self.assertEqual(self.bank.writes, 0)
      self.assertEqual(self.main.reads, 2)
      self.assertEqual(self.main.writes, 0)
      self.assertEqual(self.main.last_addr, 5)
      self.assertEqual(self.main.last_size, 8)
      self.assertEqual(self.bank.last_addr, 8)
      self.assertEqual(self.bank.last_size, 8)

      t = offset.process(True, 5, 4)
      self.assertEqual(t, 800)
      self.assertEqual(self.bank.reads, 2)
      self.assertEqual(self.bank.writes, 1)
      self.assertEqual(self.main.reads, 2)
      self.assertEqual(self.main.writes, 1)
      self.assertEqual(self.main.last_addr, 5)
      self.assertEqual(self.main.last_size, 4)
      self.assertEqual(self.bank.last_addr, 8)
      self.assertEqual(self.bank.last_size, 4)

      t = offset.process(True, 2, 8)
      self.assertEqual(t, 1600)
      self.assertEqual(self.bank.reads, 2)
      self.assertEqual(self.bank.writes, 2)
      self.assertEqual(self.main.reads, 2)
      self.assertEqual(self.main.writes, 2)
      self.assertEqual(self.main.last_addr, 2)
      self.assertEqual(self.main.last_size, 8)
      self.assertEqual(self.bank.last_addr, 5)
      self.assertEqual(self.bank.last_size, 8)

      t = offset.process(False, (1 << 32) - 6, 8)
      self.assertEqual(t, 1600)
      self.assertEqual(self.bank.reads, 3)
      self.assertEqual(self.bank.writes, 2)
      self.assertEqual(self.main.reads, 3)
      self.assertEqual(self.main.writes, 2)
      self.assertEqual(self.main.last_addr, (1 << 32) - 6)
      self.assertEqual(self.main.last_size, 8)
      self.assertEqual(self.bank.last_addr, (1 << 32) - 3)
      self.assertEqual(self.bank.last_size, 8)

