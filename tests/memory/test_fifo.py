from unittest import TestCase
from mock import Mock

from memsim.machine import MachineType
from memsim.memory.fifo import FIFO


class TestFIFO(TestCase):

    def setUp(self):
        mach = MachineType(word_size=4)
        self.mem = Mock()
        self.mem.machine = mach
        self.mem.process.return_value = 123
        self.fifo = FIFO(1, self.mem, 4, 16)
        self.fifo.set_offset(8)
        self.fifo.reset(mach)

    def test_basic(self):
        fifo = self.fifo
        mem = self.mem

        result = fifo.produce()
        self.assertEqual(result, 123)
        result = mem.process.assert_called_once_with(0, True, 8, 4)
        mem.reset_mock()

        result = fifo.produce()
        self.assertEqual(result, 123)
        mem.process.assert_called_once_with(0, True, 12, 4)
        mem.reset_mock()

        result = fifo.consume()
        self.assertEqual(result, 123)
        mem.process.assert_called_once_with(0, False, 8, 4)
        mem.reset_mock()

        result = fifo.consume()
        self.assertEqual(result, 123)
        mem.process.assert_called_once_with(0, False, 12, 4)
        mem.reset_mock()

        mem.done.return_value = 234
        result = fifo.done()
        self.assertEqual(result, 234)
        mem.done.assert_called_once_with()

    def test_empty(self):
        fifo = self.fifo
        mem = self.mem

        result = fifo.consume()
        self.assertEqual(result, -1)
        self.assertEqual(mem.process.call_count, 0)

    def test_full(self):
        fifo = self.fifo
        mem = self.mem

        ptr = 8
        for i in xrange(64 / 4):
            result = fifo.produce()
            self.assertEqual(result, 123)
            mem.process.assert_called_once_with(0, True, ptr, 4)
            mem.reset_mock()
            ptr += 4

        result = fifo.produce()
        self.assertEqual(result, -1)
        self.assertEqual(mem.process.call_count, 0)
