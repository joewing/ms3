
import unittest
from machine import MachineType
from memory.cache import Cache, CachePolicy
from mock import MockMemory

class TestCache(unittest.TestCase):

   def setUp(self):
      self.machine = MachineType(word_size = 8, addr_bits = 32)
      self.main = MockMemory()

   def test_direct(self):
      cache = Cache(self.main,
                    line_count = 4,
                    line_size = 2,
                    associativity = 1,
                    latency = 1,
                    write_back = True)
      cache.reset(self.machine)
      self.assertEqual(cache.latency, 3)

      t = cache.process(0, False, 0, 1)
      self.assertEqual(t, 203)

      t = cache.process(0, False, 1, 1)
      self.assertEqual(t, 3)

      t = cache.process(0, True, 1, 1)
      self.assertEqual(t, 3)

      t = cache.process(0, False, 8, 1)
      self.assertEqual(t, 403)

      t = cache.process(0, False, 2, 2)
      self.assertEqual(t, 203)

      t = cache.process(0, True, 4, 2)
      self.assertEqual(t, 3)

      t = cache.process(0, True, 6, 1)
      self.assertEqual(t, 203)

   def test_set(self):
      cache = Cache(self.main,
                    line_count = 4,
                    line_size = 2,
                    associativity = 2,
                    policy = CachePolicy.LRU,
                    latency = 1,
                    write_back = True)
      cache.reset(self.machine)
      self.assertEqual(cache.latency, 3)

      t = cache.process(0, False, 0, 1)
      self.assertEqual(t, 203)

      t = cache.process(0, False, 1, 1)
      self.assertEqual(t, 3)

      t = cache.process(0, True, 1, 1)
      self.assertEqual(t, 3)

      t = cache.process(0, False, 8, 1)
      self.assertEqual(t, 203)

      t = cache.process(0, False, 2, 2)
      self.assertEqual(t, 203)

      t = cache.process(0, True, 4, 2)
      self.assertEqual(t, 203)

      t = cache.process(0, True, 6, 1)
      self.assertEqual(t, 203)


