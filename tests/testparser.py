
import unittest
import lex
import memory
from . import mocks


class TestParser(unittest.TestCase):

    def test_offset(self):
        s = "(offset (value 2)(bank (join))(memory (ram (latency 100))))"
        l = lex.Lexer(mocks.MockFile(s))
        result = memory.parse_memory(l)
        self.assertEqual(str(result), s)
