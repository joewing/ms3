
import unittest
from Machine import MachineType
from Memory.DRAM import DRAM
from MockMemory import MockMemory

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

      dram.process(False, 0, 4)
      self.assertEqual(self.machine.time, 22)


