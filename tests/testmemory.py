
import unittest
from mock import MockMemory

class TestMemory(unittest.TestCase):

   def test_count(self):
      main = MockMemory()
      a = MockMemory(main)
      b = MockMemory()
      c = MockMemory(a, b)
      self.assertEquals(main.count(), 1)
      self.assertEquals(a.count(), 2)
      self.assertEquals(b.count(), 1)
      self.assertEquals(c.count(), 4)

