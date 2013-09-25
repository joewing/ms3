
import unittest
from Memory.Lexer import *
from MockFile import MockFile

class TestLexer(unittest.TestCase):

   def test_match(self):
      l = Lexer(MockFile("()asdf ( ) (123)"))
      self.assertEqual(l.get_type(), TOKEN_OPEN)
      self.assertEqual(l.get_value(), '')
      l.match(TOKEN_OPEN)
      self.assertEqual(l.get_type(), TOKEN_CLOSE)
      l.match(TOKEN_CLOSE)
      self.assertEqual(l.get_type(), TOKEN_LITERAL)
      self.assertEqual(l.get_value(), "asdf")
      l.match(TOKEN_LITERAL)
      l.match(TOKEN_OPEN)
      l.match(TOKEN_CLOSE)
      l.match(TOKEN_OPEN)
      self.assertEqual(l.get_value(), "123")
      l.match(TOKEN_LITERAL)
      l.match(TOKEN_CLOSE)
      l.match(TOKEN_EOF)

