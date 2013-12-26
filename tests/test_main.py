
import unittest

from memsim.machine import MachineType
from memsim.__main__ import (
    get_initial_memory,
)
from tests.mocks import MockBenchmark, MockMemory


class TestMemsim(unittest.TestCase):

    def test_get_initial_memory1(self):

        class MockDB:
            def get_best(self):
                return '(ram (latency 100))', 10, 20

        result = get_initial_memory(MockDB(), None, [], '.')
        self.assertEqual(str(result[0]), '(ram (latency 100))')
        self.assertEqual(result[1], 10)

    def test_get_initial_memory2(self):

        class MockDB:
            def get_best(self):
                return None, 0, 0

        class MockModel:
            machine = MachineType()
            benchmarks = [MockBenchmark([])]
            memory = MockMemory()

        dists = [None]
        result = get_initial_memory(MockDB(), MockModel(), dists, '.')
        self.assertEqual(str(result[0]), '(mock)')
        self.assertEqual(result[1], 0)

    def test_run_experiment(self):
        pass

    def test_start_experiment(self):
        pass
