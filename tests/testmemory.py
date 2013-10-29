
import unittest
from mocks import MockMemory


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

    def test_path(self):
        main = MockMemory()
        self.assertEqual(main.get_path_length(), 0)

    def test_cost(self):
        main = MockMemory()
        self.assertEqual(main.get_cost(), 0)

    def test_simplify(self):
        main = MockMemory()
        simplified = main.simplify()
        self.assertEqual(main, simplified)
