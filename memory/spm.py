
import base
import cacti
import machine
import parser

def random_spm(machine, nxt, rand, cost):
   for i in range(100):
      size = machine.word_size << rand.randint(4, 20)
      spm = SPM(nxt, size)
      spm.reset(machine)
      if spm.get_cost() <= cost:
         return spm
   return None

class SPM(base.Container):

   def __init__(self, mem, size = 0, latency = 2):
      base.Container.__init__(self, mem)
      self.size = size
      self.latency = latency

   def __str__(self):
      result  = "(spm "
      result += "(size " + str(self.size) + ")"
      result += "(latency " + str(self.latency) + ")"
      result += "(memory " + str(self.mem) + ")"
      result += ")"
      return result

   def get_cost(self):
      if self.machine.target == machine.TargetType.SIMPLE:
         return self.size * 8
      elif self.machine.target == machine.TargetType.ASIC:
         return cacti.get_area(self.machine, self)
      elif self.machine.target == machine.TargetType.FPGA:
         width = self.machine.word_size * 8
         depth = self.size // self.machine.word_size
         return machine.get_bram_count(width, depth)
      else:
         assert(False)

   def permute(self, rand, max_cost):
      if self.size > self.machine.word_size and rand.randint(0, 1) == 1:
         self.size //= 2
      else:
         self.size *= 2
         if self.get_cost() > max_cost:
            self.size //= 2
            return False
      return True

   def simplify(self):
      self.mem = self.mem.simplify()
      if self.size == 0:
         return self.mem
      else:
         return self

   def reset(self, m):
      base.Container.reset(self, m)
      if m.target == machine.TargetType.ASIC:
         self.latency = cacti.get_time(m, self)

   def push_transform(self, index, rand):
      assert(index == -1)
      rand.push_limit(self.size, self.machine.addr_mask)

   def pop_transform(self, rand):
      rand.pop_limit()

   def process(self, start, write, addr, size):
      last_addr = (addr + size) & self.machine.addr_mask
      if addr < self.size and last_addr <= self.size:
         # Complete hits the scrachpad
         offset = addr % self.machine.word_size
         count = (size + self.machine.word_size + offset - 1)
         count //= self.machine.word_size
         return start + count * self.latency
      elif addr >= self.size and last_addr > self.size:
         # Completely misses the scratchpad
         return self.mem.process(start, write, addr, size)
      elif addr > self.size and last_addr < self.size:
         # First part hits, second part misses
         msize = size - last_addr + 1
         count = (last_addr + self.machine.word_size)
         count //= self.machine.word_size
         t = start + count * self.latency
         return self.mem.process(t, write, addr, msize)
      else:
         # First part misses, second part hits
         hsize = self.size - addr;
         offset = addr % self.machine.word_size
         count = (hsize + self.machine.word_size + offset - 1)
         count //= self.machine.word_size
         t = start + count * self.latency
         return self.mem.process(t, write, self.size, size - hsize)

def _create_spm(args):
   mem = parser.get_argument(args, 'memory')
   word_count = parser.get_argument(args, 'word_count', 0)
   latency = parser.get_argument(args, 'latency', 2)
   return SPM(mem, word_count, latency)
base.constructors['spm'] = _create_spm

