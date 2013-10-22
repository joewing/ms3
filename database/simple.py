
import base

class SimpleDatabase(base.Database):
   """Simple database for running without persistence."""

   def __init__(self, m):
      base.Database.__init__(self, m)
      self.results = dict()
      self.fpga_results = dict()

   def get_result(self, mem):
      return self.results.get(mem)

   def add_result(self, mem, value):
      self.results[mem] = value

   def get_fpga_result(self, key):
      return self.fpga_results.get(key)

   def add_fpga_result(self, key, frequency, bram_count):
      self.fpga_results[key] = (frequency, bram_count)

