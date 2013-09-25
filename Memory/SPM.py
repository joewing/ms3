
from Memory import Memory
from Machine import *

def random_spm(machine, nxt, rand, cost):
   while True:
      size = machine.word_size << rand.randint(0, 8)
      spm = SPM(machine, nxt, size)
      if spm.get_cost() <= cost:
         return spm

class SPM(Memory):

   def __init__(self, machine, mem, size=0, latency=2):
      self.mem = mem
      self.addr_mask = machine.addr_mask
      self.word_size = machine.word_size
      self.size = size
      self.latency = latency

   def __str__(self):
      result  = "(spm "
      result += "(size " + str(self.size) + ")"
      result += "(memory " + str(self.mem) + ")"
      result += ")"
      return result

   def get_next(self):
      return self.mem

   def set_next(self, n):
      self.mem = n

   def get_cost(self):
      return self.size * 8

   def permute(self, rand, max_cost):
      if self.size > self.word_size and rand.randint(0, 1) == 1:
         self.size /= 2
      else:
         self.size *= 2
         if self.get_cost() > max_cost:
            self.size /= 2
            return False
      return True

   def done(self):
      return self.mem.done()

   def process(self, access):
      addr = get_address(access)
      size = get_size(access)
      last_addr = (addr + size) & self.addr_mask
      if addr < self.size and last_addr <= self.size:
         # Complete hits the scrachpad
         offset = addr % self.word_size
         count = (size + self.word_size + offset - 1) / self.word_size
         return count * self.latency + get_cycles(access)
      elif addr >= self.size and last_addr > self.size:
         # Completely misses the scratchpad
         return self.mem.process(access)
      elif addr > self.size and last_addr < self.size:
         # First part hits, second part misses
         msize = size - last_addr + 1
         count = (last_addr + self.word_size) / self.word_size
         t = count * self.latency
         updated = clone_access(access, size = msize)
         return t + self.mem.process(updated)
      else:
         # First part misses, second part hits
         hsize = self.size - addr;
         offset = addr % self.word_size
         count = (hsize + self.word_size + offset - 1) / self.word_size
         t = count * self.latency
         updated = clone_access(access,
                                address = self.size,
                                size = size - hsize)
         return t + self.mem.process(updated)

