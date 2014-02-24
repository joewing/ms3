
import unittest

from memsim import lex, memory
from tests import mocks


class TestParser(unittest.TestCase):

    def test_offset(self):
        s = '(offset (value 2)(bank (join))(memory (ram)))'
        l = lex.Lexer(mocks.MockFile(s))
        result = memory.parse_memory(l)
        expected = '(offset (value 2)(bank (join))(memory (main)))'
        self.assertEqual(str(result), expected)
