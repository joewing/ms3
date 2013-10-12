
import unittest
import lex
import machine
import memory
import mock
from memory.cache import Cache, CachePolicy

class TestCache(unittest.TestCase):

   def setUp(self):
      self.machine = machine.MachineType()
      self.main = mock.MockMemory()

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

   def testsimplify1(self):
      cache = Cache(self.main,
                    line_count = 4,
                    line_size = 2,
                    associativity = 2,
                    policy = CachePolicy.LRU,
                    latency = 1,
                    write_back = True)
      simplified = cache.simplify()
      self.assertEqual(cache, simplified)

   def test_parse(self):
      s  = "(cache (line_count 2)(line_size 4)(associativity 2)(latency 2)"
      s += "(policy fifo)(write_back false)(memory (ram (latency 100))))"
      l = lex.Lexer(mock.MockFile(s))
      result = memory.parse_memory(l)
      self.assertEqual(str(result), s)

