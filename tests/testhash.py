
import unittest
from benchmarks.hash import Hash
from process import AccessType

class TestHash(unittest.TestCase):

   def test_hash(self):
      h = Hash(1, 2, 1, 2)
      h.reset(1024)
      gen = h.run()

      t, addr, size = next(gen)
      self.assertEqual(t, AccessType.CONSUME)
      self.assertEqual(addr, 1)

      t, addr, size = next(gen)
      self.assertEqual(t, AccessType.READ)
      self.assertGreaterEqual(addr, 1024)
      self.assertEqual(size, 4)

      t, addr, size = next(gen)
      self.assertEqual(t, AccessType.PRODUCE)
      self.assertEqual(addr, 2)

      t, addr, size = next(gen)
      self.assertEqual(t, AccessType.CONSUME)
      self.assertEqual(addr, 1)

      t, addr, size = next(gen)
      self.assertEqual(t, AccessType.READ)
      self.assertGreaterEqual(addr, 1024)
      self.assertEqual(size, 4)

      t, addr, size = next(gen)
      self.assertEqual(t, AccessType.PRODUCE)
      self.assertEqual(addr, 2)

