
import unittest
from machine import MachineType
from memory.dram import DRAM

class TestDRAM(unittest.TestCase):

   def setUp(self):
      self.machine = MachineType(word_size = 4, addr_bits = 32)
      self.machine.reset()

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

      t = dram.process(0, False, 0, 4) # Miss
      self.assertEqual(t, 22)
      self.machine.time += t

      t = dram.process(0, False, 4, 4)  # Hit
      self.assertEqual(t, 8)
      self.machine.time += t

      t = dram.process(0, False, 2097152, 4)  # Miss
      self.assertEqual(t, 22)
      self.machine.time += t

      t = dram.process(0, True, 2097152, 8)   # Hit
      self.assertEqual(t, 16)
      self.machine.time += t

      t = dram.process(0, True, 2097152 - 4, 8)  # Miss/hit
      self.assertEqual(t, 30)
      self.machine.time += t

      t = dram.process(0, False, 4, 4)  # Miss/write-back
      self.assertEqual(t, 24)
      self.machine.time += t

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

      t = dram.process(0, False, 0, 4)
      self.assertEqual(t, 14)
      self.machine.time += t

      t = dram.process(0, False, 4, 4)
      self.assertEqual(t, 22)
      self.machine.time += t

      t = dram.process(0, False, 2097152, 4)
      self.assertEqual(t, 14)
      self.machine.time += t

      t = dram.process(0, True, 2097152, 8)
      self.assertEqual(t, 46)
      self.machine.time += t

      t = dram.process(0, True, 2097152 - 4, 8)
      self.assertEqual(t, 28)
      self.machine.time += t

      t = dram.process(0, False, 4, 4)
      self.assertEqual(t, 14)
      self.machine.time += t

