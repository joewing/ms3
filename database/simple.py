
import base

class SimpleDatabase(base.Database):
   """Simple database for running without persistence."""

   def __init__(self, m):
      base.Database.__init__(self, m)
      self.results = dict()

   def get_result(self, mem):
      return self.results.get(mem)

   def add_result(self, mem, value):
      self.results[mem] = value

