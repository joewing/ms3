
import unittest
from machine import MachineType
from memory.prefetch import Prefetch
from mock import MockMemory

class TestPrefetch(unittest.TestCase):

   def setUp(self):
      self.machine = MachineType()
      self.machine.reset()
      self.main = MockMemory()

   def test_positive(self):
      pf = Prefetch(self.main, 8)
      pf.reset(self.machine)

      t = pf.process(0, False, 0, 1)
      self.machine.time += t
      self.assertEqual(t, 100)
      self.assertEqual(self.main.last_addr, 8)
      self.assertEqual(self.main.last_size, 1)

      t = pf.process(0, False, 0, 1)
      self.machine.time += t
      self.assertEqual(t, 200)
      self.assertEqual(self.main.last_addr, 8)
      self.assertEqual(self.main.last_size, 1)

      t = pf.process(0, True, 16, 4)
      self.machine.time += t
      self.assertEqual(t, 500)
      self.assertEqual(self.main.last_addr, 16)
      self.assertEqual(self.main.last_size, 4)

