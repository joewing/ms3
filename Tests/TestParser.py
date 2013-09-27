
import unittest
import Machine
import MemoryParser
import MockFile

class TestParser(unittest.TestCase):

   def test_offset(self):
      m = Machine.MachineType(word_size = 8, addr_bits = 32)
      s = "(offset (value 2)(bank (join))(memory (ram (latency 100))))"
      p = MemoryParser.Parser(m, MockFile.MockFile(s))
      result = p.parse()
      self.assertEqual(str(result), s)

