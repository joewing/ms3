import unittest
from mock import Mock, patch

from memsim.machine import MachineType
from memsim.memory import MemoryList
from memsim.__main__ import (
    get_initial_memory,
    optimize,
    run_experiment,
)
from tests.mocks import MockBenchmark, MockMemory


class TestMemsim(unittest.TestCase):

    def test_get_initial_memory1(self):

        mock_model = Mock()
        mock_model.memory = MockMemory()
        mock_db = Mock()
        mock_db.get_best.return_value = '(main (ram))', 10, 20
        mock_db.load.return_value = {'use_prefetch': True}
        dists = [Mock()]

        result = get_initial_memory(mock_db, mock_model, dists, '.', 'name')

        self.assertEqual(len(result), 3)
        self.assertEqual(str(result[0]), '(main (ram (latency 100)))')
        self.assertEqual(result[1], 10)
        self.assertEqual(result[2], True)
        dists[0].load.assert_called_once_with({'use_prefetch': True}, 0)
        mock_db.get_best.assert_called_once_with(mock_model)
        mock_db.load.assert_called_once_with(mock_model)

    def test_get_initial_memory2(self):

        mock_db = Mock()
        mock_db.get_best.return_value = None, 0, 0
        mock_model = Mock()
        mock_model.machine = MachineType()
        mock_model.benchmarks = [MockBenchmark()]
        mock_model.fifos = []
        mock_model.memory = MemoryList(MockMemory())
        dists = [Mock()]

        result = get_initial_memory(mock_db, mock_model, dists, '.', 'name')

        self.assertEqual(len(result), 3)
        self.assertEqual(str(result[0]), '(main (mock)) (mock)')
        self.assertEqual(result[1], 0)
        self.assertEqual(result[2], False)

    @patch('memsim.__main__.MemoryOptimizer', autospec=True)
    @patch('memsim.__main__.get_initial_memory', autospec=True)
    def test_optimize1(self, mock_init, mock_opt):
        """Test the typical case."""

        result_count = [2]

        def mock_get_result_count(mod):
            temp = result_count[0]
            result_count[0] += 1
            return temp

        mock_db = Mock()
        mock_db.get_best.return_value = '(mock)', 1, 2
        mock_db.get_result_count.side_effect = mock_get_result_count
        mock_model = Mock()
        mock_model.machine = MachineType()
        mock_model.fifos = []
        mock_model.benchmarks = [MockBenchmark()]
        mock_init.return_value = Mock(), 10, 20

        optimize(mock_db, mock_model, 10, 5, '.', 'name')

        self.assertEqual(mock_init.call_count, 1)
        self.assertEqual(mock_db.get_best.call_count, 9)
        self.assertEqual(mock_opt.call_count, 1)

    @patch('memsim.__main__.MemoryOptimizer', autospec=True)
    @patch('memsim.__main__.get_initial_memory', autospec=True)
    def test_optimize2(self, mock_init, mock_opt):
        """Test the case where the optimizer returns None (conflict)."""

        mock_db = Mock()
        mock_db.get_best.return_value = '(mock)', 1, 2
        mock_db.get_result_count.return_value = 2
        mock_model = Mock()
        mock_model.machine = MachineType()
        mock_model.fifos = []
        mock_model.benchmarks = [MockBenchmark()]
        mock_init.return_value = Mock(), 10, 20
        mock_opt(1, 2, 3, 4, 5).optimize.return_value = None
        mock_opt.reset_mock()

        optimize(mock_db, mock_model, 10, 5, '.', 'name')

        self.assertEqual(mock_init.call_count, 1)
        self.assertEqual(mock_db.get_best.call_count, 1)
        self.assertEqual(mock_opt.call_count, 1)

    @patch('memsim.database.set_instance', autospec=True)
    @patch('memsim.__main__.optimize', autospec=True)
    def test_run_experiment1(self, mock_opt, mock_set):
        mock_db = Mock()
        result = run_experiment(mock_db, 2, 3, 4, 5, 6)
        self.assertEqual(result, mock_db.ident)
        mock_set.assert_called_once_with(mock_db)
        mock_opt.assert_called_once_with(mock_db, 2, 3, 4, 5, 6)

    @patch('memsim.database.set_instance', autospec=True)
    @patch('memsim.__main__.optimize', autospec=True)
    def test_run_experiment2(self, mock_opt, mock_set):
        mock_db = Mock()
        mock_opt.side_effect = KeyboardInterrupt()
        result = run_experiment(mock_db, 2, 3, 4, 5, 6)
        self.assertEqual(result, -1)
        mock_set.assert_called_once_with(mock_db)
        mock_opt.assert_called_once_with(mock_db, 2, 3, 4, 5, 6)

    @patch('memsim.database.set_instance', autospec=True)
    @patch('memsim.__main__.optimize', autospec=True)
    def test_run_experiment3(self, mock_opt, mock_set):
        mock_db = Mock()
        mock_opt.side_effect = Exception()
        result = run_experiment(mock_db, 2, 3, 4, 5, 6)
        self.assertEqual(result, mock_db.ident)
        mock_set.assert_called_once_with(mock_db)
        mock_opt.assert_called_once_with(mock_db, 2, 3, 4, 5, 6)

    def test_start_experiment(self):
        pass

    def test_experiment_done(self):
        pass

    def test_handle_term(self):
        pass

    def test_main(self):
        pass
