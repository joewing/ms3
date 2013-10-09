
import base
import cacti
import lex
import machine
import parser

class CachePolicy:
   LRU = 0
   MRU = 1
   FIFO = 2
   PLRU = 3
   MAX_POLICY = 3

policy_map = {
   CachePolicy.LRU:  "lru",
   CachePolicy.MRU:  "mru",
   CachePolicy.FIFO: "fifo",
   CachePolicy.PLRU: "plru"
}

def show_policy(value):
   return policy_map.get(value, "?")

def parse_policy(name):
   for (k, v) in policy_map.items():
      if name == v:
         return k
   raise lex.ParseError("invalid cache policy: " + name)

def random_cache(machine, nxt, rand, cost):
   line_size = machine.word_size
   line_count = 16
   associativity = 1
   policy = CachePolicy.LRU
   write_back = True
   result = Cache(nxt,
                  line_count = line_count,
                  line_size = line_size,
                  associativity = associativity,
                  policy = policy,
                  write_back = write_back)
   result.reset(machine)
   if result.get_cost() <= cost:
      return result
   else:
      return None

class CacheLine:
   tag = -1
   age = 0
   dirty = False

class Cache(base.Container):

   def __init__(self, mem,
                line_count = 1,
                line_size = 8,
                associativity = 1,
                latency = 1,
                policy = CachePolicy.LRU,
                write_back = True):
      base.Container.__init__(self, mem)
      self.line_count = line_count
      self.line_size = line_size
      self.associativity = associativity
      self.latency = latency
      self.policy = policy
      self.write_back = write_back
      self.lines = list()

   def __str__(self):
      result  = "(cache "
      result += "(line_count " + str(self.line_count) + ")"
      result += "(line_size " + str(self.line_size) + ")"
      result += "(associativity " + str(self.associativity) + ")"
      result += "(latency " + str(self.latency) + ")"
      if self.associativity > 1:
         result += "(policy " + show_policy(self.policy) + ")"
      if self.write_back:
         result += "(write_back true)"
      else:
         result += "(write_back false)"
      result += "(memory " + str(self.mem) + ")"
      result += ")"
      return result

   def get_cost(self):

      # Determine the width of the cache.
      index_bits = machine.log2(self.line_count - 1)
      word_size = self.machine.word_size
      line_words = (self.line_size + word_size - 1) // word_size
      ls_bits = machine.log2(line_words - 1)
      tag_bits = self.machine.addr_bits - index_bits - ls_bits
      width = 1 + tag_bits
      if self.associativity > 1:
         if self.policy == CachePolicy.PLRU:
            width += 1
         else:
            width += machine.log2(self.associativity - 1)
      if self.write_back:
         width += 1
      width *= self.associativity

      # Determine the depth of the cache.
      depth = self.line_count / self.associativity

      # Get the cost.
      if self.machine.target == machine.TargetType.SIMPLE:
         return width * depth
      elif self.machine.target == machine.TargetType.ASIC:
         return cacti.get_area(self.machine, self)
      elif self.machine.target == machine.TargetType.FPGA:
         return machine.get_bram_count(width, depth)

   def permute(self, rand, max_cost):
      param_count = 8
      param = rand.randint(0, param_count - 1)
      line_size = self.line_size
      line_count = self.line_count
      associativity = self.associativity
      policy = self.policy
      write_back = self.write_back
      for i in range(param_count):
         if param == 0:
            self.line_size *= 2
            if self.get_cost() <= max_cost: return True
            self.line_size = line_size
         elif param == 1 and line_size > self.machine.word_size:
            self.line_size //= 2
            if self.get_cost() <= max_cost: return True
            self.line_size = line_size
         elif param == 2:
            self.line_count *= 2
            if self.get_cost() <= max_cost: return True
            self.line_count = line_count
         elif param == 3 and line_count > associativity:
            self.line_count //= 2
            if self.get_cost() <= max_cost: return True
            self.line_count = line_count
         elif param == 4 and associativity < line_count:
            self.associativity *= 2
            if self.get_cost() <= max_cost: return True
            self.associativity = associativity
         elif param == 5 and associativity > 1:
            self.associativity //= 2
            if self.get_cost() <= max_cost: return True
            self.assocativity = associativity
         elif param == 6:
            self.policy = rand.randint(0, CachePolicy.MAX_POLICY)
            if self.get_cost() <= max_cost: return True
            self.policy = policy
         else:
            self.write_back = not self.write_back
            if self.get_cost() <= max_cost: return True
            self.write_back = write_back
         param = (param + 1) % param_count
      return False

   def reset(self, m):
      base.Container.reset(self, m)
      self.lines = list()
      for i in range(self.line_count):
         self.lines.append(CacheLine())
      if m.target == machine.TargetType.ASIC:
         self.latency = cacti.get_time(m, self)
      else:
         if self.policy == CachePolicy.PLRU:
            self.latency = 3 + self.associativity // 8
         else:
            self.latency = 3 + self.associativity // 4

   def process(self, start, write, addr, size):
      extra = size // self.line_size
      mask = self.machine.addr_mask
      temp = addr
      result = start
      for i in range(extra):
         result = self._do_process(result, write, temp, self.line_size)
         temp = (temp + self.line_size) & mask
      if size > extra * self.line_size:
         result = self._do_process(result, write, temp,
                                   size - extra * self.line_size)
      return result

   def _do_process(self, start, write, addr, size):
      tag = addr & ~(self.line_size - 1)
      set_size = self.line_count // self.associativity
      word_addr = addr // self.line_size
      first_line = word_addr % set_size

      # Update ages
      age_sum = 0
      for i in range(self.associativity):
         line_index = first_line + i * set_size
         line = self.lines[line_index]
         age_sum += line.age
         if self.policy != CachePolicy.PLRU:
            line.age += 1

      # Check if this address is in the cache.
      to_replace = self.lines[first_line]
      age = to_replace.age
      for i in range(self.associativity):
         line_index = first_line + i * set_size
         line = self.lines[line_index]
         if tag == line.tag: # Hit
            if self.policy == CachePolicy.PLRU:
               if age_sum + 1 == self.associativity:
                  for j in range(self.associativity):
                     self.lines[first_line + j * set_size].age = 0
               line.age = 1
            elif self.policy != CachePolicy.FIFO:
               line.age = 0
            if (not write) or self.write_back:
               line.dirty = line.dirty or write
               return self.latency
            else:
               t = self.mem.process(True, tag, self.line_size)
               return t + self.latency
         elif self.policy == CachePolicy.MRU:
            if line.age < age:
               to_replace = line
               age = line.age
         elif self.policy == CachePolicy.PLRU:
            if line.age == 0:
               to_replace = line
               age = line.age
         else:
            if line.age > age:
               to_replace = line
               age = line.age

      # If we got here we have a cache miss.
      # to_replace will be the line we need to replace.
      if (not write) or self.write_back:

         # Evict this entry if necessary.
         time = start + self.latency
         if to_replace.dirty:
            time = self.mem.process(time, True, to_replace.tag, self.line_size)
            to_replace.dirty = False
         to_replace.tag = tag
         to_replace.dirty = write

         # Update the age.
         if self.policy == CachePolicy.PLRU:
            if age_sum + 1 == self.associativity:
               for j in range(self.associativity):
                  self.lines[first_line + j * set_size].age = 0
            to_replace.age = 1
         else:
            to_replace.age = 0

         # Read the new entry.
         if (not write) or size != self.line_size:
            time = self.mem.process(time, False, tag, self.line_size)

         return time

      else:
         # Write on a write-through cache.
         return self.mem.process(start, write, addr, size) + self.latency

def _create_cache(args):
   mem = parser.get_argument(args, 'mem')
   line_count = parser.get_argument(args, 'line_count', 1)
   line_size = parser.get_argument(args, 'line_size', 8)
   associativity = parser.get_argument(args, 'associativity', 1)
   latency = parser.get_argument(args, 'latency', 1)
   policy = parse_policy(parser.get_argument(args, 'policy', 'lru'))
   write_back = parser.get_argument(args, 'write_back', True)
   return Cache(mem = mem,
                line_count = line_count,
                line_size = line_size,
                associativity = associativity,
                latency = latency,
                policy = policy,
                write_back = write_back)
base.constructors['cache'] = _create_cache

