
import base
import cacti
import machine
import parser

def random_spm(machine, nxt, rand, cost):
   size = machine.word_size * 16
   spm = SPM(nxt, size)
   spm.reset(machine)
   while spm.get_cost() < cost:
      spm.size *= 2
      spm.reset(machine)
      if spm.get_cost() > cost:
         spm.size //= 2
         spm.reset(machine)
         break
   if spm.get_cost() <= cost:
      return spm
   return None

class SPM(base.Container):

   def __init__(self, mem, size = 0,
                access_time = 2,
                cycle_time = 2):
      base.Container.__init__(self, mem)
      self.size = size
      self.access_time = access_time
      self.cycle_time = cycle_time
      self.pending = 0

   def __str__(self):
      result  = "(spm "
      result += "(size " + str(self.size) + ")"
      result += "(access_time " + str(self.access_time) + ")"
      result += "(cycle_time " + str(self.cycle_time) + ")"
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
      self.pending = 0
      if m.target == machine.TargetType.ASIC:
         self.access_time = cacti.get_access_time(m, self)
         self.cycle_time = cacti.get_cycle_time(m, self)

   def push_transform(self, index, rand):
      assert(index == -1)
      rand.push_limit(self.size, self.machine.addr_mask)

   def pop_transform(self, rand):
      rand.pop_limit()

   def get_path_length(self):
      return self.machine.addr_bits + self.get_next().get_path_length()

   def done(self):
      return max(self.pending - self.machine.time, 0)

   def process(self, start, write, addr, size):
      result = max(start, self.pending - self.machine.time)
      last_addr = (addr + size) & self.machine.addr_mask
      if addr < self.size and last_addr <= self.size:
         # Complete hits the scrachpad
         offset = addr % self.machine.word_size
         count = (size + self.machine.word_size + offset - 1)
         count //= self.machine.word_size
         self.pending = self.machine.time + result
         self.pending += max(self.cycle_time - self.access_time, 0)
         result += (count - 1) * self.cycle_time + self.access_time
      elif addr >= self.size and last_addr > self.size:
         # Completely misses the scratchpad
         self.pending = self.machine.time + result
         result = self.mem.process(result, write, addr, size)
      elif addr > self.size and last_addr < self.size:
         # First part hits, second part misses
         msize = size - last_addr + 1
         count = (last_addr + self.machine.word_size)
         count //= self.machine.word_size
         result += (count - 1) * self.cycle_time + self.access_time
         self.pending = self.machine.time + result
         self.pending += max(self.cycle_time - self.access_time, 0)
         result = self.mem.process(result, write, addr, msize)
      else:
         # First part misses, second part hits
         hsize = self.size - addr;
         offset = addr % self.machine.word_size
         count = (hsize + self.machine.word_size + offset - 1)
         count //= self.machine.word_size
         result += (count - 1) * self.cycle_time + self.access_time
         self.pending = self.machine.time + result
         self.pending += max(self.cycle_time - self.access_time, 0)
         result = self.mem.process(result, write, self.size, size - hsize)
      return result

def _create_spm(lexer, args):
   mem = parser.get_argument(lexer, args, 'memory')
   size = parser.get_argument(lexer, args, 'size', 0)
   access_time = parser.get_argument(lexer, args, 'access_time', 2)
   cycle_time = parser.get_argument(lexer, args, 'cycle_time', access_time)
   return SPM(mem, size, access_time, cycle_time)
base.constructors['spm'] = _create_spm

