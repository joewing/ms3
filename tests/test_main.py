
import unittest

from memsim.machine import MachineType
from memsim.__main__ import (
    get_initial_memory,
)
from tests.mocks import MockBenchmark, MockMemory


class TestMemsim(unittest.TestCase):

    def test_get_initial_memory1(self):

        class MockDB:
            def get_best(self, mod):
                return '(ram)', 10, 20

            def load(self, mod):
                state = {'use_prefetch': True}
                return state

        class MockDist:
            def load(self, state, index):
                assert(index == 0)

        class MockModel:
            memory = MockMemory()

        dists = [MockDist()]
        result = get_initial_memory(MockDB(), MockModel(), dists, '.')
        self.assertEqual(str(result[0]), '(mock)')
        self.assertEqual(result[1], 10)

    def test_get_initial_memory2(self):

        class MockDB:
            def get_best(self, mod):
                return None, 0, 0

            def save(self, mod, state):
                pass

        class MockModel:
            machine = MachineType()
            benchmarks = [MockBenchmark([])]
            fifos = []
            memory = MockMemory()

        class MockDist:
            def save(self, state, index):
                assert(index == 0)

        dists = [MockDist()]
        result = get_initial_memory(MockDB(), MockModel(), dists, '.')
        self.assertEqual(str(result[0]), '(mock)')
        self.assertEqual(result[1], 0)

    def test_run_experiment(self):
        pass

    def test_start_experiment(self):
        pass
