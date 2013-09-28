
from Memory import Memory

def random_spm(machine, nxt, rand, cost):
   while True:
      size = machine.word_size << rand.randint(0, 8)
      spm = SPM(nxt, size)
      if spm.get_cost() <= cost:
         return spm

class SPM(Memory):

   def __init__(self, mem, size = 0, latency = 2):
      self.mem = mem
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
      if self.size > self.machine.word_size and rand.randint(0, 1) == 1:
         self.size /= 2
      else:
         self.size *= 2
         if self.get_cost() > max_cost:
            self.size /= 2
            return False
      return True

   def push_transform(self, index, rand):
      assert(index == -1)
      rand.push_limit(self.size, self.machine.addr_mask)

   def pop_transform(self, rand):
      rand.pop_limit()

   def done(self):
      return self.mem.done()

   def process(self, write, addr, size):
      last_addr = (addr + size) & self.machine.addr_mask
      if addr < self.size and last_addr <= self.size:
         # Complete hits the scrachpad
         offset = addr % self.machine.word_size
         count = (size + self.machine.word_size + offset - 1)
         count /= self.machine.word_size
         return count * self.latency
      elif addr >= self.size and last_addr > self.size:
         # Completely misses the scratchpad
         return self.mem.process(write, addr, size)
      elif addr > self.size and last_addr < self.size:
         # First part hits, second part misses
         msize = size - last_addr + 1
         count = (last_addr + self.machine.word_size)
         count /= self.machine.word_size
         t = count * self.latency
         return t + self.mem.process(write, addr, msize)
      else:
         # First part misses, second part hits
         hsize = self.size - addr;
         offset = addr % self.machine.word_size
         count = (hsize + self.machine.word_size + offset - 1)
         count /= self.machine.word_size
         t = count * self.latency
         return t + self.mem.process(write, self.size, size - hsize)

