
import unittest
import Lexer
import MockFile

class TestLexer(unittest.TestCase):

   def test_match(self):
      l = Lexer.Lexer(MockFile.MockFile("()asdf ( ) (123)"))
      self.assertEqual(l.get_type(), Lexer.TOKEN_OPEN)
      self.assertEqual(l.get_value(), '')
      l.match(Lexer.TOKEN_OPEN)
      self.assertEqual(l.get_type(), Lexer.TOKEN_CLOSE)
      l.match(Lexer.TOKEN_CLOSE)
      self.assertEqual(l.get_type(), Lexer.TOKEN_LITERAL)
      self.assertEqual(l.get_value(), "asdf")
      l.match(Lexer.TOKEN_LITERAL)
      l.match(Lexer.TOKEN_OPEN)
      l.match(Lexer.TOKEN_CLOSE)
      l.match(Lexer.TOKEN_OPEN)
      self.assertEqual(l.get_value(), "123")
      l.match(Lexer.TOKEN_LITERAL)
      l.match(Lexer.TOKEN_CLOSE)
      l.match(Lexer.TOKEN_EOF)

