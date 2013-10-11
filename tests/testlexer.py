
import unittest
import lex
import mock

class TestLexer(unittest.TestCase):

   def test_match(self):
      l = lex.Lexer(mock.MockFile("()asdf ( ) (123)"), "mock")
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

