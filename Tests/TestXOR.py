
import unittest
from Machine import MachineType
from Memory.Join import Join
from Memory.XOR import XOR
from MockMemory import MockMemory

class TestXOR(unittest.TestCase):

   def setUp(self):
      self.machine = MachineType(word_size = 8, addr_bits = 32)
      self.main = MockMemory()
      self.bank = MockMemory(Join())

   def test_xor16(self):
      xor = XOR(self.bank, self.main, 16)
      xor.reset(self.machine)

      t = xor.process(False, 32, 8)
      self.assertEqual(t, 1600)
      self.assertEqual(self.bank.reads, 1)
      self.assertEqual(self.bank.writes, 0)
      self.assertEqual(self.main.reads, 1)
      self.assertEqual(self.main.writes, 0)
      self.assertEqual(self.main.last_addr, 32)
      self.assertEqual(self.main.last_size, 8)
      self.assertEqual(self.bank.last_addr, 48)
      self.assertEqual(self.bank.last_size, 8)


