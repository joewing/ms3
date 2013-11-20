
from memsim.database import base


class SimpleDatabase(base.Database):
    """Simple database for running without persistence."""

    def __init__(self):
        base.Database.__init__(self)
        self.fpga_results = dict()
        self.cacti_results = dict()

    def get_fpga_result(self, key):
        return self.fpga_results.get(key)

    def add_fpga_result(self, key, frequency, bram_count):
        self.fpga_results[key] = (frequency, bram_count)

    def get_cacti_result(self, key):
        return self.cacti_results.get(key)

    def add_cacti_result(self, key, access_time, cycle_time, area):
        self.cacti_results[key] = (access_time, cycle_time, area)
