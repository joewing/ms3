
from __future__ import print_function
import json

from memsim.database import base


class SharedDatabase(base.BaseDatabase):

    def __init__(self, name, request_queue, response_queue):
        base.BaseDatabase.__init__(self)
        self.name = name
        self.request_queue = request_queue
        self.response_queue = response_queue

    def _execute(self, func, *args):
        request = (func, args)
        self.request_queue.put(request)
        return self.response_queue.get()

    def update_status(self, best_value, best_cost, evaluation, status):
        return self._execute('update_status', self.name,
                             best_value, best_cost, evaluation, status)

    def signal_exit(self):
        return self._execute('signal_exit', self.name)

    def load(self, mod):
        return self._execute('load', str(mod))

    def save(self, mod, state):
        return self._execute('save', str(mod), json.dumps(state))

    def get_result(self, mod, mem):
        return self._execute('get_result', str(mod), str(mem))

    def add_result(self, mod, mem, value, cost):
        return self._execute('add_result', str(mod), str(mem), value, cost)

    def get_best(self, mod):
        return self._execute('get_best', str(mod))

    def get_result_count(self, mod):
        return self._execute('get_result_count', str(mod))

    def get_fpga_result(self, name):
        return self._execute('get_fpga_result', name)

    def add_fpga_result(self, name, frequency, bram_count):
        return self._execute('add_fpga_result', name, frequency, bram_count)

    def get_cacti_result(self, name):
        return self._execute('get_cacti_result', name)

    def add_cacti_result(self, name, access_time, cycle_time, area):
        return self._execute('add_cacti_result', name, access_time,
                             cycle_time, area)
