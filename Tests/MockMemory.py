

from Memory.Memory import Memory

class MockMemory(Memory):

   def __init__(self, mem = None, *banks):
      self.mem = mem
      self.banks = list(banks)

   def __str__(self):
      result = "(mock "
      if self.mem != None: result += str(self.mem)
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

