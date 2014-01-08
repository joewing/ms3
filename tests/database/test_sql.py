
from unittest import TestCase

from memsim.database.sql import SQLDatabase


class TestSQLDatabase(TestCase):

    def setUp(self):
        self.db = SQLDatabase('sqlite:///')
        connected = self.db.connect()
        self.assertTrue(connected)

    def test_load_empty(self):
        state = self.db.load('test')
        self.assertEqual(state, {})

    def test_load_save(self):
        state = {'value': 'key'}
        self.db.save('test', state)

        loaded_state = self.db.load('test')
        self.assertEqual(state, loaded_state)

        empty_state = self.db.load('test2')
        self.assertEqual(empty_state, {})

    def test_get_empty_result(self):
        result = self.db.get_result('model', 'mem')
        self.assertEqual(result, None)

    def test_get_result_dup(self):
        result = self.db.get_result('a', 'b')
        self.assertEqual(result, None)
        result = self.db.get_result('a', 'b')
        self.assertEqual(result, -1)

    def test_add_get_result(self):
        self.db.add_result('model1', 'mem1', 1, 2)
        result = self.db.get_result('model1', 'mem1')
        self.assertEqual(result, 1)

    def test_get_best(self):

        # No results.
        result = self.db.get_best('m')
        self.assertEqual(result, (None, 0, 0))

        # Single result.
        self.db.add_result('m', 'abc', 10, 20)
        result = self.db.get_best('m')
        self.assertEqual(result, ('abc', 10, 20))

        # Longer name.
        self.db.add_result('m', 'abcd', 10, 20)
        result = self.db.get_best('m')
        self.assertEqual(result, ('abc', 10, 20))

        #  Shorter name.
        self.db.add_result('m', 'ab', 10, 20)
        result = self.db.get_best('m')
        self.assertEqual(result, ('ab', 10, 20))

        # Better cost.
        self.db.add_result('m', 'xyz', 10, 19)
        result = self.db.get_best('m')
        self.assertEqual(result, ('xyz', 10, 19))

        # Better value.
        self.db.add_result('m', 'asdf', 9, 20)
        result = self.db.get_best('m')
        self.assertEqual(result, ('asdf', 9, 20))

        # Wrong model.
        result = self.db.get_best('m2')
        self.assertEqual(result, (None, 0, 0))

    def test_get_result_count(self):

        count = self.db.get_result_count('m')
        self.assertEqual(count, 0)

        self.db.add_result('m', 'a', 1, 2)
        count = self.db.get_result_count('m')
        self.assertEqual(count, 1)

        self.db.add_result('m2', 'b', 3, 4)
        count = self.db.get_result_count('m')
        self.assertEqual(count, 1)

        self.db.add_result('m', 'c', 3, 4)
        count = self.db.get_result_count('m')
        self.assertEqual(count, 2)

    def test_fpga_result(self):

        result = self.db.get_fpga_result('1')
        self.assertEqual(result, None)

        self.db.add_fpga_result('2', 3, 4)
        result = self.db.get_fpga_result('1')
        self.assertEqual(result, None)

        self.db.add_fpga_result('1', 4, 5)
        result = self.db.get_fpga_result('1')
        self.assertEqual(result, (4, 5))

    def test_cacti_result(self):

        result = self.db.get_cacti_result('1')
        self.assertEqual(result, None)

        self.db.add_cacti_result('2', 3, 4, 5)
        result = self.db.get_cacti_result('1')
        self.assertEqual(result, None)

        self.db.add_cacti_result('1', 4, 5, 6)
        result = self.db.get_cacti_result('1')
        self.assertEqual(result, (4, 5, 6))

    def test_get_status(self):

        result = len(list(self.db.get_status()))
        self.assertEqual(result, 0)

        self.db.add_result('m1', 'a', 1, 2)
        self.db.add_result('m2', 'b', 2, 3)
        self.db.add_result('m1', 'c', 4, 5)
        result = self.db.get_status()
        result = sorted(list(result))
        expected = [('m1', 2, 1), ('m2', 1, 2)]
        self.assertEqual(result, expected)

    def test_get_states(self):

        result = len(list(self.db.get_states()))
        self.assertEqual(result, 0)

        self.db.add_result('m1', 'a', 1, 2)
        self.db.add_result('m2', 'b', 5, 6)
        result = self.db.get_states()
        result = sorted(list(result))
        expected = [(1, 'm1'), (2, 'm2')]
        self.assertEqual(result, expected)
