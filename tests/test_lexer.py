
import unittest

from memsim import lex
from tests import mocks


class TestLexer(unittest.TestCase):

    def test_match(self):
        l = lex.Lexer(mocks.MockFile("()asdf ( ) (123)"))
        self.assertEqual(l.get_type(), lex.TOKEN_OPEN)
        self.assertEqual(l.get_value(), '')
        l.match(lex.TOKEN_OPEN)
        self.assertEqual(l.get_type(), lex.TOKEN_CLOSE)
        l.match(lex.TOKEN_CLOSE)
        self.assertEqual(l.get_type(), lex.TOKEN_LITERAL)
        self.assertEqual(l.get_value(), "asdf")
        l.match(lex.TOKEN_LITERAL)
        l.match(lex.TOKEN_OPEN)
        l.match(lex.TOKEN_CLOSE)
        l.match(lex.TOKEN_OPEN)
        self.assertEqual(l.get_value(), "123")
        l.match(lex.TOKEN_LITERAL)
        l.match(lex.TOKEN_CLOSE)
        l.match(lex.TOKEN_EOF)
