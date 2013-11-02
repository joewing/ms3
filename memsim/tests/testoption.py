
import unittest

from memsim import lex, machine, memory
from memsim.memory.option import Option
from memsim.tests import mocks


class TestOption(unittest.TestCase):

    def setUp(self):
        self.machine = machine.MachineType()

    def test_option1(self):
        op = Option()
        main0 = mocks.MockMemory()
        main1 = mocks.MockMemory()
        op.add_option(main0)
        op.add_option(main1)
        op.reset(self.machine)

        t = op.process(0, False, 8, 1)
        self.assertEqual(t, 100)
        self.assertEqual(main0.reads, 1)
        self.assertEqual(main1.reads, 0)

    def test_parse(self):
        s = "(option (memory0 (ram (latency 100)))"
        s += "(memory1 (ram (latency 200))))"
        l = lex.Lexer(mocks.MockFile(s))
        result = memory.parse_memory(l)
        self.assertEqual(str(result), "(ram (latency 100))")

    def test_cost(self):
        op = Option()
        op.add_option(mocks.MockMemory())
        self.assertEqual(op.get_cost(), 0)

    def test_path(self):
        op = Option()
        op.add_option(mocks.MockMemory())
        self.assertEqual(op.get_path_length(), 0)
