
from Memory import Memory
from Join import Join, find_join

def random_offset(machine, nxt, rand, cost):
   if rand.randbool():
      offset = rand.randint(-machine.word_size, machine.word_size)
   else:
      offset = rand.random_address(machine.word_size)
   join = Join()
   result = Offset(join, nxt, offset)
   join.parent = result
   return result

class Offset(Memory):

   def __init__(self, bank, mem, offset=0):
      self.bank = bank
      self.mem = mem
      self.offset = offset

   def __str__(self):
      result  = '(offset '
      result += '(value ' + str(self.offset) + ')'
      result += '(bank ' + str(self.bank) + ')'
      result += '(memory ' + str(self.mem) + ')'
      result += ')'
      return result

   def get_next(self):
      return self.mem

   def set_next(self, n):
      self.mem = n

   def get_banks(self):
      return [self.bank]

   def set_bank(self, i, b):
      assert(i == 0)
      self.bank = b

   def simplify(self):
      if isinstance(self.bank, Join):
         return self.mem
      if self.offset == 0:
         last = None
         t = self.bank
         while (not isinstance(t, Join)) or t.parent != self:
            last = t
            t = t.get_next()
         assert(last != None) # We already checked for an empty bank.
         last.set_next(self.mem)
         return self.bank
      return self

   def permute(self, rand, max_cost):
      word_size = self.machine.word_size
      if rand.randbool():
         self.offset = rand.randint(-word_size, word_size)
      else:
         self.offset = rand.random_address(word_size)
      return True

   def push_transform(self, index, rand):
      if index == 0:
         rand.push_transform(lambda a: a + self.offset)
      else:
         rand.push_transform(lambda a: a)

   def pop_transform(self, rand):
      rand.pop_transform()

   def done(self):
      return self.mem.done()

   def process(self, write, addr, size):
      addr = (addr + self.offset) & self.machine.addr_mask
      return self.bank.process(write, addr, size)

   def forward(self, index, write, addr, size):
      assert(index == 0)
      addr = (addr - self.offset) & self.machine.addr_mask
      return self.mem.process(write, addr, size) 

