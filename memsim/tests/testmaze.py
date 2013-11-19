
import unittest

from memsim import benchmarks, lex
from memsim.benchmarks.maze import Maze
from memsim.process import AccessType
from memsim.tests import mocks


class TestMaze(unittest.TestCase):

    def test_maze1(self):
        m = Maze(2, 2, 3)
        m.reset(1024, '', 0, 0)
        gen = m.run()

        expected = [
            (AccessType.WRITE, 1024, 1),
            (AccessType.WRITE, 1044, 1),
            (AccessType.WRITE, 1025, 1),
            (AccessType.WRITE, 1045, 1),
            (AccessType.WRITE, 1026, 1),
            (AccessType.WRITE, 1046, 1),
            (AccessType.WRITE, 1027, 1),
            (AccessType.WRITE, 1047, 1),
            (AccessType.WRITE, 1028, 1),
            (AccessType.WRITE, 1048, 1),
            (AccessType.WRITE, 1029, 1),
            (AccessType.WRITE, 1030, 1),
            (AccessType.WRITE, 1031, 1),
            (AccessType.WRITE, 1032, 1),
            (AccessType.WRITE, 1033, 1),
            (AccessType.WRITE, 1034, 1),
            (AccessType.WRITE, 1035, 1),
            (AccessType.WRITE, 1036, 1),
            (AccessType.WRITE, 1037, 1),
            (AccessType.WRITE, 1038, 1),
            (AccessType.WRITE, 1039, 1),
            (AccessType.WRITE, 1040, 1),
            (AccessType.WRITE, 1041, 1),
            (AccessType.WRITE, 1042, 1),
            (AccessType.WRITE, 1043, 1),
            (AccessType.WRITE, 1052, 1),
            (AccessType.READ, 1052, 1),
            (AccessType.WRITE, 1036, 1),
            (AccessType.READ, 1041, 1),
            (AccessType.READ, 1046, 1),
            (AccessType.READ, 1040, 1),
            (AccessType.WRITE, 1036, 1),
            (AccessType.READ, 1037, 1),
            (AccessType.READ, 1038, 1),
        ]

        for e in expected:
            actual = next(gen)
            self.assertEqual(actual, e)

    def test_parse(self):
        s = "(maze (seed 1)(width 2)(height 3))"
        l = lex.Lexer(mocks.MockFile(s))
        result = benchmarks.parse_benchmark(l)
        self.assertEqual(str(result), s)
