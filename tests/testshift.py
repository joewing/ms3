
import unittest
from machine import MachineType
from memory import Join
from memory.shift import Shift
from mock import MockMemory

class TestShift(unittest.TestCase):

   def setUp(self):
      self.machine = MachineType()
      self.main = MockMemory()
      self.join = Join()
      self.bank = MockMemory(self.join)

   def test_positive(self):
      shift = Shift(self.bank, self.main, 1)
      self.join.parent = shift
      shift.reset(self.machine)

      t = shift.process(0, False, 0, 8)
      self.assertEqual(t, 1600)
      self.assertEqual(self.bank.reads, 1)
      self.assertEqual(self.bank.writes, 0)
      self.assertEqual(self.main.reads, 1)
      self.assertEqual(self.main.writes, 0)
      self.assertEqual(self.main.last_addr, 0)
      self.assertEqual(self.main.last_size, 8)
      self.assertEqual(self.bank.last_addr, 0)
      self.assertEqual(self.bank.last_size, 8)

      t = shift.process(0, False, 1, 1)
      self.assertEqual(t, 200)
      self.assertEqual(self.bank.reads, 2)
      self.assertEqual(self.bank.writes, 0)
      self.assertEqual(self.main.reads, 2)
      self.assertEqual(self.main.writes, 0)
      self.assertEqual(self.main.last_addr, 1)
      self.assertEqual(self.main.last_size, 1)
      self.assertEqual(self.bank.last_addr, 1)
      self.assertEqual(self.bank.last_size, 1)

      t = shift.process(0, True, 1, 1)
      self.assertEqual(t, 200)
      self.assertEqual(self.bank.reads, 2)
      self.assertEqual(self.bank.writes, 1)
      self.assertEqual(self.main.reads, 2)
      self.assertEqual(self.main.writes, 1)
      self.assertEqual(self.main.last_addr, 1)
      self.assertEqual(self.main.last_size, 1)
      self.assertEqual(self.bank.last_addr, 1)
      self.assertEqual(self.bank.last_size, 1)

      t = shift.process(0, False, 16, 8)
      self.assertEqual(t, 1600)
      self.assertEqual(self.bank.reads, 3)
      self.assertEqual(self.bank.writes, 1)
      self.assertEqual(self.main.reads, 3)
      self.assertEqual(self.main.writes, 1)
      self.assertEqual(self.main.last_addr, 16)
      self.assertEqual(self.main.last_size, 8)
      self.assertEqual(self.bank.last_addr, 32)
      self.assertEqual(self.bank.last_size, 8)

      t = shift.process(0, False, 1 << 31, 4)
      self.assertEqual(t, 800)
      self.assertEqual(self.bank.reads, 4)
      self.assertEqual(self.bank.writes, 1)
      self.assertEqual(self.main.reads, 4)
      self.assertEqual(self.main.writes, 1)
      self.assertEqual(self.main.last_addr, 1 << 31)
      self.assertEqual(self.main.last_size, 4)
      self.assertEqual(self.bank.last_addr, 8)
      self.assertEqual(self.bank.last_size, 4)

      t = shift.process(0, False, 105, 2)
      self.assertEqual(t, 400)
      self.assertEqual(self.bank.reads, 5)
      self.assertEqual(self.bank.writes, 1)
      self.assertEqual(self.main.reads, 5)
      self.assertEqual(self.main.writes, 1)
      self.assertEqual(self.main.last_addr, 105)
      self.assertEqual(self.main.last_size, 2)
      self.assertEqual(self.bank.last_addr, 209)
      self.assertEqual(self.bank.last_size, 2)

   def test_negative(self):
      shift = Shift(self.bank, self.main, -2)
      self.join.parent = shift
      shift.reset(self.machine)

      t = shift.process(0, False, 0, 8)
      self.assertEqual(t, 1600)
      self.assertEqual(self.bank.reads, 1)
      self.assertEqual(self.bank.writes, 0)
      self.assertEqual(self.main.reads, 1)
      self.assertEqual(self.main.writes, 0)
      self.assertEqual(self.main.last_addr, 0)
      self.assertEqual(self.main.last_size, 8)
      self.assertEqual(self.bank.last_addr, 0)
      self.assertEqual(self.bank.last_size, 8)

      t = shift.process(0, False, 1, 1)
      self.assertEqual(t, 200)
      self.assertEqual(self.bank.reads, 2)
      self.assertEqual(self.bank.writes, 0)
      self.assertEqual(self.main.reads, 2)
      self.assertEqual(self.main.writes, 0)
      self.assertEqual(self.main.last_addr, 1)
      self.assertEqual(self.main.last_size, 1)
      self.assertEqual(self.bank.last_addr, 1)
      self.assertEqual(self.bank.last_size, 1)

      t = shift.process(0, True, 1, 1)
      self.assertEqual(t, 200)
      self.assertEqual(self.bank.reads, 2)
      self.assertEqual(self.bank.writes, 1)
      self.assertEqual(self.main.reads, 2)
      self.assertEqual(self.main.writes, 1)
      self.assertEqual(self.main.last_addr, 1)
      self.assertEqual(self.main.last_size, 1)
      self.assertEqual(self.bank.last_addr, 1)
      self.assertEqual(self.bank.last_size, 1)

      t = shift.process(0, True, 9, 4)
      self.assertEqual(t, 800)
      self.assertEqual(self.bank.reads, 2)
      self.assertEqual(self.bank.writes, 2)
      self.assertEqual(self.main.reads, 2)
      self.assertEqual(self.main.writes, 2)
      self.assertEqual(self.main.last_addr, 9)
      self.assertEqual(self.main.last_size, 4)
      self.assertEqual(self.bank.last_addr, (1 << 30) | 1)
      self.assertEqual(self.bank.last_size, 4)

