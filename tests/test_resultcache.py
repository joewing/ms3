
import unittest

from memsim import resultcache


class TestResultCache(unittest.TestCase):

    def test_dict(self):
        """Test that the cache works as a dictionary."""
        cache = resultcache.ResultCache(10)

        for x in xrange(10):
            cache[x] = 10 - x
        for x in xrange(10):
            self.assertEqual(cache[x], 10 - x)

    def test_insert(self):
        """Test that inserting removes the oldest."""
        cache = resultcache.ResultCache(3)

        for x in xrange(5):
            cache[x] = x + 1

        self.assertEqual(cache[0], None)
        self.assertEqual(cache[1], None)
        self.assertEqual(cache[2], 3)
        self.assertEqual(cache[3], 4)
        self.assertEqual(cache[4], 5)

    def test_get(self):
        """Test that get updates the cache."""
        cache = resultcache.ResultCache(3)

        cache[1] = 5
        cache[2] = 10
        cache[3] = 15
        self.assertEqual(cache[1], 5)

        cache[4] = 20
        self.assertEqual(cache[1], 5)
        self.assertEqual(cache[2], None)
        self.assertEqual(cache[3], 15)
        self.assertEqual(cache[4], 20)

        cache[0] = 25
        self.assertEqual(cache[0], 25)
        self.assertEqual(cache[1], None)
        self.assertEqual(cache[2], None)
        self.assertEqual(cache[3], 15)
        self.assertEqual(cache[4], 20)

    def test_in(self):
        """Test the 'in' operator."""
        cache = resultcache.ResultCache(2)

        self.assertFalse(1 in cache)
        self.assertFalse(2 in cache)
        self.assertFalse(3 in cache)
        self.assertFalse(4 in cache)

        cache[2] = 4
        cache[3] = 6
        self.assertFalse(1 in cache)
        self.assertTrue(2 in cache)
        self.assertTrue(3 in cache)
        self.assertFalse(4 in cache)

        cache[4] = 8
        self.assertFalse(1 in cache)
        self.assertFalse(2 in cache)
        self.assertTrue(3 in cache)
        self.assertTrue(4 in cache)

        cache[1] = 8
        self.assertTrue(1 in cache)
        self.assertFalse(2 in cache)
        self.assertFalse(3 in cache)
        self.assertTrue(4 in cache)
