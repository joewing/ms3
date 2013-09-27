
import unittest
from Machine import MachineType
from Memory.SPM import SPM
from MockMemory import MockMemory

class TestSPM(unittest.TestCase):

   def setUp(self):
      self.machine =  MachineType(word_size = 8, addr_bits = 32)
      self.main = MockMemory()

