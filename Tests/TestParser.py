
import unittest
import Lexer
import MemoryParser
import MockFile

class TestParser(unittest.TestCase):

   def test_offset(self):
      s = "(offset (value 2)(bank (join))(memory (ram (latency 100))))"
      l = Lexer.Lexer(MockFile.MockFile(s))
      result = MemoryParser.parse_memory(l)
      self.assertEqual(str(result), s)

