
import unittest
from Machine import MachineType
from Memory.Cache import Cache, CachePolicy
from MockMemory import MockMemory

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

      t = cache.process(False, 0, 1)
      self.assertEqual(t, 201)

      t = cache.process(False, 1, 1)
      self.assertEqual(t, 1)

      t = cache.process(True, 1, 1)
      self.assertEqual(t, 1)

      t = cache.process(False, 8, 1)
      self.assertEqual(t, 401)

      t = cache.process(False, 2, 2)
      self.assertEqual(t, 201)

      t = cache.process(True, 4, 2)
      self.assertEqual(t, 1)

      t = cache.process(True, 6, 1)
      self.assertEqual(t, 201)

   def test_set(self):
      cache = Cache(self.main,
                    line_count = 4,
                    line_size = 2,
                    associativity = 2,
                    policy = CachePolicy.LRU,
                    latency = 1,
                    write_back = True)

      cache.reset(self.machine)

      t = cache.process(False, 0, 1)
      self.assertEqual(t, 201)

      t = cache.process(False, 1, 1)
      self.assertEqual(t, 1)

      t = cache.process(True, 1, 1)
      self.assertEqual(t, 1)

      t = cache.process(False, 8, 1)
      self.assertEqual(t, 201)

      t = cache.process(False, 2, 2)
      self.assertEqual(t, 201)

      t = cache.process(True, 4, 2)
      self.assertEqual(t, 201)

      t = cache.process(True, 6, 1)
      self.assertEqual(t, 201)


