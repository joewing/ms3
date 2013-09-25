
import unittest
from Memory.Lexer import *

class MockFile:

   def __init__(self, value):
      self.value = value
      self.index = 0

   def read(self, size):
      if self.index + size >= len(self.value):
         size = len(self.value) - self.index + 1
      result = self.value[self.index:self.index + size]
      self.index += size
      return result

class TestLexer(unittest.TestCase):

   def setUp(self):
      pass

   def test_match(self):
      try:
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
      except ParseError:
         self.fail("unexpected ParseError")

