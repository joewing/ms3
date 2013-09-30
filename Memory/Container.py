
from Memory import Memory

class Container(Memory):
   """A memory containing another memory (caches, etc.)."""

   def __init__(self, mem):
      self.mem = mem

   def get_next(self):
      return self.mem

   def set_next(self, n):
      self.mem = n

   def done(self):
      return self.mem.done()

