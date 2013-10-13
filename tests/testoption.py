
import unittest
import lex
import machine
import memory
import mock
from memory.option import Option

class TestOption(unittest.TestCase):

   def setUp(self):
      self.machine = machine.MachineType()

   def test_option1(self):
      op = Option()
      main0 = mock.MockMemory()
      main1 = mock.MockMemory()
      op.add_option(main0)
      op.add_option(main1)
      op.reset(self.machine)

      t = op.process(0, False, 8, 1)
      self.assertEqual(t, 100)
      self.assertEqual(main0.reads, 1)
      self.assertEqual(main1.reads, 0)

   def test_parse(self):
      s  = "(option (memory0 (ram (latency 100)))"
      s += "(memory1 (ram (latency 200))))"
      l = lex.Lexer(mock.MockFile(s))
      result = memory.parse_memory(l)
      self.assertEqual(str(result), "(ram (latency 100))")

   def test_cost(self):
      op = Option()
      op.add_option(mock.MockMemory())
      self.assertEqual(op.get_cost(), 0)

   def test_path(self):
      op = Option()
      op.add_option(mock.MockMemory())
      self.assertEqual(op.get_path_length(), 0)

