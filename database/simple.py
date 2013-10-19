
import base

class SimpleDatabase(base.Database):

   def __init__(self, m):
      base.Database.__init__(self, m)
      self.results = dict()
      self.data = dict()

   def get_result(self, mem):
      return self.results.get(mem)

   def add_result(self, mem, value):
      self.results[mem] = value

   def set_value(self, key, value):
      self.data[key] = value

   def get_value(self, key, default = None):
      return self.data.get(key, default)

