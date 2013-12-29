
from __future__ import print_function
import json

from memsim.database import base


class SharedDatabase(base.BaseDatabase):

    def __init__(self, ident, name, request_queue, response_queue):
        base.BaseDatabase.__init__(self)
        self.ident = ident
        self.name = name
        self.request_queue = request_queue
        self.response_queue = response_queue

    def _execute(self, func, needs_response, *args):
        request = (func, needs_response, args)
        self.request_queue.put(request)
        if needs_response:
            return self.response_queue.get()
        else:
            return None

    def update_status(self, best_value, best_cost, evaluation, status):
        return self._execute('update_status', False, self.name,
                             best_value, best_cost, evaluation, status)

    def load(self, mod):
        return self._execute('load', True, str(mod))

    def save(self, mod, state):
        return self._execute('save', False, str(mod), json.dumps(state))

    def get_result(self, mod, mem):
        return self._execute('get_result', True, str(mod), str(mem))

    def add_result(self, mod, mem, value, cost):
        return self._execute('add_result', False,
                             str(mod), str(mem), value, cost)

    def get_best(self, mod):
        return self._execute('get_best', True, str(mod))

    def get_result_count(self, mod):
        return self._execute('get_result_count', True, str(mod))

    def get_fpga_result(self, name):
        return self._execute('get_fpga_result', True, name)

    def add_fpga_result(self, name, frequency, bram_count):
        return self._execute('add_fpga_result', False,
                             name, frequency, bram_count)

    def get_cacti_result(self, name):
        return self._execute('get_cacti_result', True, name)

    def add_cacti_result(self, name, access_time, cycle_time, area):
        return self._execute('add_cacti_result', False, name, access_time,
                             cycle_time, area)
