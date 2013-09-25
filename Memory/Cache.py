
from Memory import Memory
from Machine import *

class CachePolicy:

   LRU = 0
   MRU = 1
   FIFO = 2
   PLRU = 3
   MAX_POLICY = 3

   def show(self, value):
      if value == LRU:
         return "lru"
      elif value == MRU:
         return "mru"
      elif value == FIFO:
         return "fifo"
      elif value == PLRU:
         return "plru"
      else:
         return "?"

class Cache(Memory):

   # Mapping line -> (tag, age, dirty)
   data = list()

   def __init__(self, machine, mem,
                line_count = 1,
                line_size = 8,
                associativity = 1,
                latency = 1,
                policy = CachePolicy.LRU,
                write_back = True):
      self.machine = machine
      self.mem = mem
      self.line_count = line_count
      self.line_size = line_size
      self.associativity = associativity
      self.latency = latency
      self.policy = policy
      self.write_back = write_back

   def __str__(self):
      result  = "(cache "
      result += "(line_count " + str(self.line_count) + ")"
      result += "(line_size " + str(self.line_size) + ")"
      result += "(associativity " + str(self.associativity) + ")"
      result += "(latency " + str(self.latency) + ")"
      if self.associativity > 1:
         result += "(policy " + CachePolicy.show(self.policy) + ")"
      if self.write_back:
         result += "(write_back true)"
      else:
         result += "(write_back false)"
      result += "(memory " + str(self.mem) + ")"
      result += ")"
      return result

   def get_next(self):
      return self.mem

   def set_next(self, n):
      self.mem = n

   def get_cost(self):
      return self.line_count * self.line_size * self.machine.word_size * 8

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
         elif param == 1 and line_size > machine.word_size:
            self.line_size /= 2
            if self.get_cost() <= max_cost: return True
            self.line_size = line_size
         elif param == 2:
            self.line_count *= 2
            if self.get_cost() <= max_cost: return True
            self.line_count = line_count
         elif param == 3 and line_count > associativity:
            self.line_count /= 2
            if self.get_cost() <= max_cost: return True
            self.line_count = line_count
         elif param == 4 and associativity < line_count:
            self.associativity *= 2
            if self.get_cost() <= max_cost: return True
            self.associativity = associativity
         elif param == 5 and associativity > 1:
            self.associativity /= 2
            if self.get_cost() <= max_cost: return True
            self.assocativity = associativity
         elif param == 6:
            self.policy = self.rand.randint(0, CachePolicy.MAX_POLICY)
            if self.get_cost() <= max_cost: return True
            self.policy = policy
         else:
            self.write_back = !self.write_back
            if self.get_cost() <= max_cost: return True
            self.write_back = write_back
         param = (param + 1) % param_count
      return False

   def reset(self):
      self.mem.reset()
      data = list()

   def done(self):
      return self.mem.done()


   def process(self, access):
      addr = get_address(access)
      size = get_size(access)
      is_write = is_write(access)
      tag = addr & ~(self.line_size - 1)
      set_size = self.line_count / self.associativity




