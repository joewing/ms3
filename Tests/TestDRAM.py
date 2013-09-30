
import unittest
from Machine import MachineType
from Memory.DRAM import DRAM

class TestDRAM(unittest.TestCase):

   def setUp(self):
      self.machine = MachineType(word_size = 4, addr_bits = 32)

   def test_open(self):
      dram = DRAM(multiplier = 2,
                  cas_cycles = 2,
                  rcd_cycles = 3,
                  rp_cycles  = 4,
                  wb_cycles  = 1,
                  page_size  = 1024,
                  page_count = 2048,
                  width      = 2,
                  burst_size = 2,
                  open_page_mode = True)
      dram.reset(self.machine)

      dram.process(False, 0, 4) # Miss
      t = 22
      self.assertEqual(self.machine.time, t)

      dram.process(False, 4, 4)  # Hit
      t += 8
      self.assertEqual(self.machine.time, t)

      dram.process(False, 2097152, 4)  # Miss
      t += 22
      self.assertEqual(self.machine.time, t)

      dram.process(True, 2097152, 8)   # Hit
      t += 16
      self.assertEqual(self.machine.time, t)

      dram.process(True, 2097152 - 4, 8)  # Miss/hit
      t += 30
      self.assertEqual(self.machine.time, t)

      dram.process(False, 4, 4)  # Miss/write-back
      t += 24
      self.assertEqual(self.machine.time, t)

   def test_closed(self):
      dram = DRAM(multiplier = 2,
                  cas_cycles = 2,
                  rcd_cycles = 3,
                  rp_cycles  = 4,
                  wb_cycles  = 1,
                  page_size  = 1024,
                  page_count = 2048,
                  width      = 2,
                  burst_size = 2,
                  open_page_mode = False)
      dram.reset(self.machine)

      dram.process(False, 0, 4)
      t = 14
      self.assertEqual(self.machine.time, t)

      dram.process(False, 4, 4)
      t += 22
      self.assertEqual(self.machine.time, t)

      dram.process(False, 2097152, 4)
      t += 14
      self.assertEqual(self.machine.time, t)

      dram.process(True, 2097152, 8)
      t += 46
      self.assertEqual(self.machine.time, t)

      dram.process(True, 2097152 - 4, 8)
      t += 28
      self.assertEqual(self.machine.time, t)

      dram.process(False, 4, 4)
      t += 14
      self.assertEqual(self.machine.time, t)

