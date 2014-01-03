
from unittest import TestCase
from mock import patch

from memsim.model import Model
from memsim.sim.base import evaluate
from tests.mocks import MockBenchmark, MockMemory


class MockMemoryList(object):

    def __init__(self):
        self.memories = [MockMemory(), MockMemory()]

    def get_cost(self):
        return 10


class MockProcessList(object):

    benchmarks = []

    def add_benchmark(self, b, s):
        self.benchmarks.append((b, s))

    def run(self, ml):
        return 5


class EvaluateTestCase(TestCase):

    @patch('memsim.sim.base.ProcessList', autospec=True)
    def test_evaluate(self, mock_pl):
        pl = MockProcessList()
        mock_pl.return_value = pl
        mod = Model()
        mod.benchmarks = [MockBenchmark(), MockBenchmark()]
        ml = MockMemoryList()
        result = evaluate(mod, ml, '/')
        self.assertEqual(result, (5, 10))
        self.assertEqual(len(pl.benchmarks), 2)
