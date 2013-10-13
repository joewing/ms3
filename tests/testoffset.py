
import unittest
import lex
import machine
import memory
import mock
from memory.offset import Offset

class TestOffset(unittest.TestCase):

   def setUp(self):
      self.machine = machine.MachineType()
      self.main = mock.MockMemory()
      self.bank = mock.MockMemory(memory.Join())

   def test_positive(self):
      offset = Offset(self.bank, self.main, 3)
      offset.reset(self.machine)

      t = offset.process(0, False, 0, 8)
      self.assertEqual(t, 1600)
      self.assertEqual(self.bank.reads, 1)
      self.assertEqual(self.bank.writes, 0)
      self.assertEqual(self.main.reads, 1)
      self.assertEqual(self.main.writes, 0)
      self.assertEqual(self.main.last_addr, 0)
      self.assertEqual(self.main.last_size, 8)
      self.assertEqual(self.bank.last_addr, 3)
      self.assertEqual(self.bank.last_size, 8)

      t = offset.process(0, False, 5, 8)
      self.assertEqual(t, 1600)
      self.assertEqual(self.bank.reads, 2)
      self.assertEqual(self.bank.writes, 0)
      self.assertEqual(self.main.reads, 2)
      self.assertEqual(self.main.writes, 0)
      self.assertEqual(self.main.last_addr, 5)
      self.assertEqual(self.main.last_size, 8)
      self.assertEqual(self.bank.last_addr, 8)
      self.assertEqual(self.bank.last_size, 8)

      t = offset.process(0, True, 5, 4)
      self.assertEqual(t, 800)
      self.assertEqual(self.bank.reads, 2)
      self.assertEqual(self.bank.writes, 1)
      self.assertEqual(self.main.reads, 2)
      self.assertEqual(self.main.writes, 1)
      self.assertEqual(self.main.last_addr, 5)
      self.assertEqual(self.main.last_size, 4)
      self.assertEqual(self.bank.last_addr, 8)
      self.assertEqual(self.bank.last_size, 4)

      t = offset.process(0, True, 2, 8)
      self.assertEqual(t, 1600)
      self.assertEqual(self.bank.reads, 2)
      self.assertEqual(self.bank.writes, 2)
      self.assertEqual(self.main.reads, 2)
      self.assertEqual(self.main.writes, 2)
      self.assertEqual(self.main.last_addr, 2)
      self.assertEqual(self.main.last_size, 8)
      self.assertEqual(self.bank.last_addr, 5)
      self.assertEqual(self.bank.last_size, 8)

      t = offset.process(0, False, (1 << 32) - 6, 8)
      self.assertEqual(t, 1600)
      self.assertEqual(self.bank.reads, 3)
      self.assertEqual(self.bank.writes, 2)
      self.assertEqual(self.main.reads, 3)
      self.assertEqual(self.main.writes, 2)
      self.assertEqual(self.main.last_addr, (1 << 32) - 6)
      self.assertEqual(self.main.last_size, 8)
      self.assertEqual(self.bank.last_addr, (1 << 32) - 3)
      self.assertEqual(self.bank.last_size, 8)

   def test_simplify1(self):
      offset = Offset(memory.Join(), self.main, 0)
      offset.reset(self.machine)
      simplified = offset.simplify()
      self.assertEqual(str(simplified), "(mock)")

   def test_simplify2(self):
      offset = Offset(self.bank, self.main, 0)
      offset.reset(self.machine)
      simplified = offset.simplify()
      self.assertEqual(str(simplified), "(mock (mock))")

   def test_simplify3(self):
      offset = Offset(self.bank, self.main, 1)
      offset.reset(self.machine)
      simplified = offset.simplify()
      self.assertEqual(str(simplified),
                       "(offset (value 1)(bank (mock (join)))(memory (mock)))")

   def test_parse(self):
      s = "(offset (value 8)(bank (join))(memory (ram (latency 100))))"
      l = lex.Lexer(mock.MockFile(s))
      result = memory.parse_memory(l)
      self.assertEqual(str(result), s)

   def test_path(self):
      offset = Offset(self.bank, self.main, 1)
      offset.reset(self.machine)
      self.assertEqual(offset.get_path_length(), self.machine.addr_bits)

   def test_cost(self):
      offset = Offset(self.bank, self.main, 1)
      self.assertEqual(offset.get_cost(), 0)

