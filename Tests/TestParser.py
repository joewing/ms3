
import unittest
from Machine import MachineType
from Memory.Parser import *
from MockFile import MockFile

class TestParser(unittest.TestCase):

   def test_offset(self):
      m = MachineType(word_size = 8, addr_bits = 32)
      s = "(offset (value 2)(bank (join))(memory (ram (latency 100))))"
      p = Parser(m, MockFile(s))
      result = p.parse()
      self.assertEqual(str(result), s)

