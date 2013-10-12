
import unittest
import machine
import mock
import memory
import lex
from memory.spm import SPM

class TestSPM(unittest.TestCase):

   def setUp(self):
      self.machine =  machine.MachineType()
      self.main = mock.MockMemory()

   def test_spm1(self):
      spm = SPM(self.main, size = 1024, latency = 1)
      spm.reset(self.machine)

      t = spm.process(0, False, 0, 1)
      self.assertEqual(t, 1)

      t = spm.process(0, False, 1024 - 8, 8)
      self.assertEqual(t, 1)

      t = spm.process(0, False, 1024, 4)
      self.assertEqual(t, 400)

      t = spm.process(0, False, 1023, 2)
      self.assertEqual(t, 101)

      t = spm.process(0, True, 1024, 1)
      self.assertEqual(t, 100)

      t = spm.process(0, True, 8192, 16)
      self.assertEqual(t, 1600)

   def test_parse(self):
      s = "(spm (size 1024)(latency 3)(memory (ram (latency 100))))"
      l = lex.Lexer(mock.MockFile(s))
      result = memory.parse_memory(l)
      self.assertEqual(str(result), s)

