
import unittest
from machine import MachineType
from memory.prefetch import Prefetch
from mock import MockMemory

class TestPrefetch(unittest.TestCase):

   def setUp(self):
      self.machine = MachineType(word_size = 4, addr_bits = 32)
      self.main = MockMemory()

   def test_positive(self):
      pf = Prefetch(self.main, 8)
      pf.reset(self.machine)

      t = pf.process(False, 0, 1)
      self.assertEqual(t, 100)
      self.assertEqual(self.main.last_addr, 8)
      self.assertEqual(self.main.last_size, 1)

      t = pf.process(False, 0, 1)
      self.assertEqual(t, 200)
      self.assertEqual(self.main.last_addr, 8)
      self.assertEqual(self.main.last_size, 1)

      t = pf.process(True, 16, 4)
      self.assertEqual(t, 500)
      self.assertEqual(self.main.last_addr, 16)
      self.assertEqual(self.main.last_size, 4)

