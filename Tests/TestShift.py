
import unittest
from Machine import MachineType, create_access, get_address, get_size
from Memory.Join import Join
from Memory.Shift import Shift
from MockMemory import *

class TestShift(unittest.TestCase):

   def setUp(self):
      self.machine = MachineType(word_size = 8, addr_bits = 32)
      self.main = MockMemory()
      self.join = Join(self.machine)
      self.bank = MockMemory(self.join)

   def test_positive(self):
      shift = Shift(self.machine, self.bank, self.main, 1)
      self.join.parent = shift

      t = shift.process(create_access(False, 0, 8))
      self.assertEqual(t, 1600)
      self.assertEqual(self.bank.reads, 1)
      self.assertEqual(self.bank.writes, 0)
      self.assertEqual(self.main.reads, 1)
      self.assertEqual(self.main.writes, 0)
      self.assertEqual(get_address(self.main.last_access), 0)
      self.assertEqual(get_size(self.main.last_access), 8)
      self.assertEqual(get_address(self.bank.last_access), 0)
      self.assertEqual(get_size(self.bank.last_access), 8)

      t = shift.process(create_access(False, 1, 1))
      self.assertEqual(t, 200)
      self.assertEqual(self.bank.reads, 2)
      self.assertEqual(self.bank.writes, 0)
      self.assertEqual(self.main.reads, 2)
      self.assertEqual(self.main.writes, 0)
      self.assertEqual(get_address(self.main.last_access), 1)
      self.assertEqual(get_size(self.main.last_access), 1)
      self.assertEqual(get_address(self.bank.last_access), 1)
      self.assertEqual(get_size(self.bank.last_access), 1)

      t = shift.process(create_access(True, 1, 1))
      self.assertEqual(t, 200)
      self.assertEqual(self.bank.reads, 2)
      self.assertEqual(self.bank.writes, 1)
      self.assertEqual(self.main.reads, 2)
      self.assertEqual(self.main.writes, 1)
      self.assertEqual(get_address(self.main.last_access), 1)
      self.assertEqual(get_size(self.main.last_access), 1)
      self.assertEqual(get_address(self.bank.last_access), 1)
      self.assertEqual(get_size(self.bank.last_access), 1)

      t = shift.process(create_access(False, 16, 8))
      self.assertEqual(t, 1600)
      self.assertEqual(self.bank.reads, 3)
      self.assertEqual(self.bank.writes, 1)
      self.assertEqual(self.main.reads, 3)
      self.assertEqual(self.main.writes, 1)
      self.assertEqual(get_address(self.main.last_access), 16)
      self.assertEqual(get_size(self.main.last_access), 8)
      self.assertEqual(get_address(self.bank.last_access), 32)
      self.assertEqual(get_size(self.bank.last_access), 8)

      t = shift.process(create_access(False, 1 << 31, 4))
      self.assertEqual(t, 800)
      self.assertEqual(self.bank.reads, 4)
      self.assertEqual(self.bank.writes, 1)
      self.assertEqual(self.main.reads, 4)
      self.assertEqual(self.main.writes, 1)
      self.assertEqual(get_address(self.main.last_access), 1 << 31)
      self.assertEqual(get_size(self.main.last_access), 4)
      self.assertEqual(get_address(self.bank.last_access), 8)
      self.assertEqual(get_size(self.bank.last_access), 4)

      t = shift.process(create_access(False, 105, 2))
      self.assertEqual(t, 400)
      self.assertEqual(self.bank.reads, 5)
      self.assertEqual(self.bank.writes, 1)
      self.assertEqual(self.main.reads, 5)
      self.assertEqual(self.main.writes, 1)
      self.assertEqual(get_address(self.main.last_access), 105)
      self.assertEqual(get_size(self.main.last_access), 2)
      self.assertEqual(get_address(self.bank.last_access), 209)
      self.assertEqual(get_size(self.bank.last_access), 2)

   def test_negative(self):
      shift = Shift(self.machine, self.bank, self.main, -2)
      self.join.parent = shift

      t = shift.process(create_access(False, 0, 8))
      self.assertEqual(t, 1600)
      self.assertEqual(self.bank.reads, 1)
      self.assertEqual(self.bank.writes, 0)
      self.assertEqual(self.main.reads, 1)
      self.assertEqual(self.main.writes, 0)
      self.assertEqual(get_address(self.main.last_access), 0)
      self.assertEqual(get_size(self.main.last_access), 8)
      self.assertEqual(get_address(self.bank.last_access), 0)
      self.assertEqual(get_size(self.bank.last_access), 8)

      t = shift.process(create_access(False, 1, 1))
      self.assertEqual(t, 200)
      self.assertEqual(self.bank.reads, 2)
      self.assertEqual(self.bank.writes, 0)
      self.assertEqual(self.main.reads, 2)
      self.assertEqual(self.main.writes, 0)
      self.assertEqual(get_address(self.main.last_access), 1)
      self.assertEqual(get_size(self.main.last_access), 1)
      self.assertEqual(get_address(self.bank.last_access), 1)
      self.assertEqual(get_size(self.bank.last_access), 1)

      t = shift.process(create_access(True, 1, 1))
      self.assertEqual(t, 200)
      self.assertEqual(self.bank.reads, 2)
      self.assertEqual(self.bank.writes, 1)
      self.assertEqual(self.main.reads, 2)
      self.assertEqual(self.main.writes, 1)
      self.assertEqual(get_address(self.main.last_access), 1)
      self.assertEqual(get_size(self.main.last_access), 1)
      self.assertEqual(get_address(self.bank.last_access), 1)
      self.assertEqual(get_size(self.bank.last_access), 1)

      t = shift.process(create_access(True, 9, 4))
      self.assertEqual(t, 800)
      self.assertEqual(self.bank.reads, 2)
      self.assertEqual(self.bank.writes, 2)
      self.assertEqual(self.main.reads, 2)
      self.assertEqual(self.main.writes, 2)
      self.assertEqual(get_address(self.main.last_access), 9)
      self.assertEqual(get_size(self.main.last_access), 4)
      self.assertEqual(get_address(self.bank.last_access), (1 << 30) | 1)
      self.assertEqual(get_size(self.bank.last_access), 4)

