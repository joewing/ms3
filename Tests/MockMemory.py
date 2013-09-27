
from Memory.Memory import Memory

class MockMemory(Memory):

   last_addr = 0
   last_size = 0
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

   def process(self, write, addr, size):
      self.last_addr = addr
      self.last_size = size
      if write:
         self.writes += 1
      else:
         self.reads += 1
      result = 100 * size
      if self.mem != None:
         result += self.mem.process(write, addr, size)
      return result

