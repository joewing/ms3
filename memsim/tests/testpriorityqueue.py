
import unittest

from .. import priorityqueue


class TestPriorityQueue(unittest.TestCase):

    def test_binary_heap(self):
        heap = priorityqueue.BinaryHeap()

        values = [(1, 0), (2, 1), (3, 2), (0, 3), (5, 4),
                  (6, 5), (7, 6), (8, 7), (9, 8), (10, 9),
                  (11, 10), (4, 11), (12, 12), (-1, 13), (-2, 14),
                  (-3, 15), (-4, 16), (-5, 17), (-8, 18), (-9, 19)]
        for v in values:
            heap.push(v[0], v[1])

        for v in sorted(values):
            self.assertFalse(heap.empty())
            self.assertEqual(heap.key(), v[0])
            self.assertEqual(heap.value(), v[1])
            heap.pop()
        self.assertTrue(heap.empty())

        heap.push(0, 1)
        self.assertFalse(heap.empty())
        self.assertEqual(heap.key(), 0)
        self.assertEqual(heap.value(), 1)
        heap.push(1, 2)
        self.assertFalse(heap.empty())
        self.assertEqual(heap.key(), 0)
        self.assertEqual(heap.value(), 1)
        heap.pop()
        self.assertFalse(heap.empty())
        self.assertEqual(heap.key(), 1)
        self.assertEqual(heap.value(), 2)
        heap.push(-1, 3)
        self.assertFalse(heap.empty())
        self.assertEqual(heap.key(), -1)
        self.assertEqual(heap.value(), 3)
        heap.pop()
        self.assertFalse(heap.empty())
        self.assertEqual(heap.key(), 1)
        self.assertEqual(heap.value(), 2)
        heap.pop()
        self.assertTrue(heap.empty())
