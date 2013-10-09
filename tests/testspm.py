
import unittest
from machine import MachineType
from memory.spm import SPM
from mock import MockMemory

class TestSPM(unittest.TestCase):

   def setUp(self):
      self.machine =  MachineType(word_size = 8, addr_bits = 32)
      self.main = MockMemory()

   def test_spm1(self):
      spm = SPM(self.main, size = 1024, latency = 1)
      spm.reset(self.machine)

      t = spm.process(0, False, 0, 1)
      self.assertEqual(t, 1)

      t = spm.process(0, False, 1024 - 8, 8)
      self.assertEqual(t, 1)

      t = spm.process(0, False, 1024, 4)
      self.assertEqual(t, 400)

      t = spm.process(0, False, 1023, 2)
      self.assertEqual(t, 101)

      t = spm.process(0, True, 1024, 1)
      self.assertEqual(t, 100)

      t = spm.process(0, True, 8192, 16)
      self.assertEqual(t, 1600)

