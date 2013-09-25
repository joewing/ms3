
import unittest
from Memory import *
from MockMemory import *

class TestMemory(unittest.TestCase):

   def test_count(self):
      main = MockMemory()
      a = MockMemory(main)
      b = MockMemory()
      c = MockMemory(a, b)
      self.assertEquals(c.count(), 4)

