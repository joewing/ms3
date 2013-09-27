
import unittest
import PriorityQueue

class TestPriorityQueue(unittest.TestCase):

   def test_binary_heap(self):
      heap = PriorityQueue.BinaryHeap()

      values = [  (1, 0), (2, 1), (3, 2), (0, 3), (5, 4),
                  (6, 5), (7, 6), (8, 7), (9, 8), (10, 9),
                  (11, 10), (4, 11), (12, 12), (-1, 13) ]
      for v in values:
         heap.push(v[0], v[1])

      for v in sorted(values):
         self.assertFalse(heap.empty())
         self.assertEqual(heap.key(), v[0])
         self.assertEqual(heap.value(), v[1])
         heap.pop()
      self.assertTrue(heap.empty())

