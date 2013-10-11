
import unittest
import lex
import memory
import mock

class TestParser(unittest.TestCase):

   def test_offset(self):
      s = "(offset (value 2)(bank (join))(memory (ram (latency 100))))"
      l = lex.Lexer(mock.MockFile(s), "mock")
      result = memory.parse_memory(l)
      self.assertEqual(str(result), s)

