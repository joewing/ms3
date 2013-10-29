
import os
import tempfile
import unittest

import benchmarks
from benchmarks.trace import Trace
import lex
import mock
from process import AccessType


class TestTrace(unittest.TestCase):

    def test_trace1(self):
        fd, file_name = tempfile.mkstemp()
        try:
            with os.fdopen(fd, 'w') as f:
                f.write("Rabc:10\n")
                f.write("W8:8\n")
                f.write("M0:4\n")
                f.write("I18:0\n")
                f.write("P1:0\n")
                f.write("C2:0\n")
                f.write("X3:0\n")
            trace = Trace(file_name)
            output = trace.run()

            t, addr, size = next(output)
            self.assertEqual(t, AccessType.READ)
            self.assertEqual(addr, 0xABC)
            self.assertEqual(size, 16)

            t, addr, size = next(output)
            self.assertEqual(t, AccessType.WRITE)
            self.assertEqual(addr, 8)
            self.assertEqual(size, 8)

            t, addr, size = next(output)
            self.assertEqual(t, AccessType.READ)
            self.assertEqual(addr, 0)
            self.assertEqual(size, 4)
            t, addr, size = next(output)
            self.assertEqual(t, AccessType.WRITE)
            self.assertEqual(addr, 0)
            self.assertEqual(size, 4)

            t, addr, size = next(output)
            self.assertEqual(t, AccessType.IDLE)
            self.assertEqual(addr, 0x18)

            t, addr, size = next(output)
            self.assertEqual(t, AccessType.PRODUCE)
            self.assertEqual(addr, 1)

            t, addr, size = next(output)
            self.assertEqual(t, AccessType.CONSUME)
            self.assertEqual(addr, 2)

            t, addr, size = next(output)
            self.assertEqual(t, AccessType.END)
            self.assertEqual(addr, 3)

            with self.assertRaises(StopIteration):
                next(output)
        finally:
            os.remove(file_name)

    def test_parse(self):
        s = "(trace (file test_file))"
        l = lex.Lexer(mock.MockFile(s))
        result = benchmarks.parse_benchmark(l)
        self.assertEqual(str(result), s)
