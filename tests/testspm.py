
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
      spm = SPM(self.main, size = 1024,
                access_time = 1,
                cycle_time = 1)
      spm.reset(self.machine)

      t = spm.process(0, False, 0, 1)
      self.assertEqual(t, 1)
      self.machine.time += t

      t = spm.process(0, False, 1024 - 8, 8)
      self.assertEqual(t, 1)
      self.machine.time += t

      t = spm.process(0, False, 1024, 4)
      self.assertEqual(t, 400)
      self.machine.time += t

      t = spm.process(0, False, 1023, 2)
      self.assertEqual(t, 101)
      self.machine.time += t

      t = spm.process(0, True, 1024, 1)
      self.assertEqual(t, 100)
      self.machine.time += t

      t = spm.process(0, True, 8192, 16)
      self.assertEqual(t, 1600)
      self.machine.time += t

   def test_parse(self):
      s  = "(spm (size 1024)(access_time 3)(cycle_time 4)"
      s += "(memory (ram (latency 100))))"
      l = lex.Lexer(mock.MockFile(s))
      result = memory.parse_memory(l)
      self.assertEqual(str(result), s)

   def test_simplify(self):
      spm = SPM(self.main, size = 1024, access_time = 1, cycle_time = 1)
      simplified = spm.simplify()
      self.assertEqual(spm, simplified)

   def test_cost(self):
      spm = SPM(self.main, size = 1024, access_time = 1, cycle_time = 1)
      spm.reset(self.machine)
      self.assertEqual(spm.get_cost(), 8 * 1024)

   def test_path(self):
      spm = SPM(self.main, size = 1024, access_time = 1, cycle_time = 1)
      spm.reset(self.machine)
      self.assertEqual(spm.get_path_length(), self.machine.addr_bits)

