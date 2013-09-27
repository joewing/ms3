
import unittest
import Machine
import Lexer
import MemoryParser
import MockFile

class TestParser(unittest.TestCase):

   def test_offset(self):
      m = Machine.MachineType(word_size = 8, addr_bits = 32)
      s = "(offset (value 2)(bank (join))(memory (ram (latency 100))))"
      l = Lexer.Lexer(MockFile.MockFile(s))
      result = MemoryParser.parse_memory(m, l)
      self.assertEqual(str(result), s)

