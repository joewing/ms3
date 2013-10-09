
import unittest
from machine import MachineType
from memory import Join
from memory.split import Split
from mock import MockMemory

class TestSplit(unittest.TestCase):

   def setUp(self):
      self.machine = MachineType(frequency = 1e9,
                                 word_size = 8,
                                 addr_bits = 32)
      self.main = MockMemory()
      self.join0 = Join(0)
      self.join1 = Join(1)
      self.bank0 = MockMemory(self.join0)
      self.bank1 = MockMemory(self.join1)

   def test_split256(self):
      split = Split(self.bank0, self.bank1,
                    self.main, offset = 256)
      self.join0.parent = split
      self.join1.parent = split

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

