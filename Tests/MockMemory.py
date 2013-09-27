
from Machine import get_size, is_write
from Memory.Memory import Memory

class MockMemory(Memory):

   last_access = None
   reads = 0
   writes = 0

   def __init__(self, mem = None, *banks):
      self.mem = mem
      self.banks = list(banks)

   def __str__(self):
      if self.mem == None:
         return "(mock)"
      result = "(mock " + str(self.mem)
      for c in self.banks:
         result += str(c)
      result += ")"
      return result

   def get_next(self):
      return self.mem

   def set_next(self, n):
      self.mem = n

   def get_banks(self):
      return self.banks

   def set_bank(self, i, b):
      assert(i < len(self.banks))
      self.banks[i] = b

   def process(self, access):
      self.last_access = access
      if is_write(access):
         self.writes += 1
      else:
         self.reads += 1
      result = 100 * get_size(access)
      if self.mem != None:
         result += self.mem.process(access)
      return result

