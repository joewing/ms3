from memsim.resultcache import ResultCache
from memsim.database import base


class SharedDatabase(base.BaseDatabase):

    def __init__(self, ident, name, request_queue, response_queue):
        base.BaseDatabase.__init__(self)
        self.ident = ident
        self.name = name
        self.request_queue = request_queue
        self.response_queue = response_queue
        self.result_cache = ResultCache(16)
        self.fpga_cache = ResultCache(1024)
        self.cacti_cache = ResultCache(1024)
        self.score_cache = ResultCache(8192)

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
        return self._execute('save', False, str(mod), state)

    def get_result(self, mod, mem, subsystem):
        result_hash = self.get_result_hash(mod, mem, subsystem)
        if result_hash in self.result_cache:
            return self.result_cache[result_hash]
        result = self._execute('get_result', True, str(mod),
                               mem, subsystem)
        if result[0] is not None and result[0] >= 0:
            self.result_cache[result_hash] = result
        return result

    def add_result(self, mod, mem, subsystem, value, trace):
        result_hash = self.get_result_hash(mod, mem, subsystem)
        self.result_cache[result_hash] = value, str(trace)
        return self._execute('add_result', False, str(mod), mem,
                             subsystem, value, str(trace))

    def get_score(self, mod, mem, full):
        s = str(mod) + str(mem)
        if full:
            s += ':full'
        score_hash = self.get_hash(s)
        if score_hash in self.score_cache:
            return self.score_cache[score_hash]
        result = self._execute('get_score', True, str(mod), str(mem), full)
        if result is not None:
            self.score_cache[score_hash] = result
        return result

    def add_score(self, mod, mem, full, score):
        s = str(mod) + str(mem)
        if full:
            s += ':full'
        score_hash = self.get_hash(s)
        self.score_cache[score_hash] = score
        return self._execute('add_score', False, str(mod), str(mem),
                             full, score)

    def insert_best(self, mod, mem, value, cost):
        return self._execute('insert_best', False, str(mod),
                             mem, value, cost)

    def update_best(self, mod, mem, value, cost):
        return self._execute('update_best', False, str(mod),
                             mem, value, cost)

    def get_best(self, mod):
        return self._execute('get_best', True, str(mod))

    def get_random(self, mod, subsystem):
        return self._execute('get_random', True, str(mod), subsystem)

    def get_result_count(self, mod):
        return self._execute('get_result_count', True, str(mod))

    def get_fpga_result(self, name):
        name_hash = self.get_hash(name)
        if name_hash in self.fpga_cache:
            return self.fpga_cache[name_hash]
        result = self._execute('get_fpga_result', True, name)
        if result is not None:
            self.fpga_cache[name_hash] = result
        return result

    def add_fpga_result(self, name, frequency, bram_count,
                        lut_count, reg_count):
        name_hash = self.get_hash(name)
        self.fpga_cache[name_hash] = (frequency, bram_count,
                                      lut_count, reg_count)
        return self._execute('add_fpga_result', False,
                             name, frequency, bram_count,
                             lut_count, reg_count)

    def get_cacti_result(self, name):
        name_hash = self.get_hash(name)
        if name_hash in self.cacti_cache:
            return self.cacti_cache[name_hash]
        result = self._execute('get_cacti_result', True, name)
        if result is not None:
            self.cacti_cache[name_hash] = result
        return result

    def add_cacti_result(self, name, access_time, cycle_time, area):
        name_hash = self.get_hash(name)
        self.cacti_cache[name_hash] = (access_time, cycle_time, area)
        return self._execute('add_cacti_result', False, name, access_time,
                             cycle_time, area)
