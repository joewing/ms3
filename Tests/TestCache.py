
import unittest
from Machine import MachineType, create_access
from Memory.Cache import Cache, CachePolicy
from MockMemory import *

class TestCache(unittest.TestCase):

   def test_direct(self):
      machine = MachineType(word_size = 8, addr_bits = 32)
      main = MockMemory()
      cache = Cache(machine, main,
                    line_count = 4,
                    line_size = 2,
                    associativity = 1,
                    latency = 1,
                    write_back = True)

      cache.reset()

      t = cache.process(create_access(False, 0, 1))
      self.assertEqual(t, 101)

      t = cache.process(create_access(False, 1, 1))
      self.assertEqual(t, 1)

      t = cache.process(create_access(True, 1, 1))
      self.assertEqual(t, 1)

      t = cache.process(create_access(False, 8, 1))
      self.assertEqual(t, 201)

      t = cache.process(create_access(False, 2, 2))
      self.assertEqual(t, 101)

      t = cache.process(create_access(True, 4, 2))
      self.assertEqual(t, 1)

      t = cache.process(create_access(True, 6, 1))
      self.assertEqual(t, 101)

