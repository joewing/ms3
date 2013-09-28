
from Memory import Memory
from Join import Join

def random_shift(machine, nxt, rand, cost):
   bits = machine.addr_bits - machine.word_bits - 1
   shift = rand.randint(-bits, bits)
   join = Join()
   result = Shift(join, nxt, shift)
   join.parent = result
   return result

class Shift(Memory):

   def __init__(self, bank, mem, shift = 0):
      self.bank = bank
      self.mem = mem
      self.shift = shift

   def __str__(self):
      result  = "(shift "
      result += "(value " + str(self.shift) + ")"
      result += '(bank ' + str(self.bank) + ')'
      result += '(memory ' + str(self.mem) + ')'
      result += ")"
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
      if self.shift == 0:
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
      bits = self.machine.addr_bits - self.machine.word_bits - 1
      self.shift = rand.randint(-bits, bits)
      return True

   def push_transform(self, index, rand):
      if index == 0:
         rand.push_transform(lambda a: self._rotate(a, self.shift))
      else:
         rand.push_transform(lambda a: a)

   def pop_transform(self, rand):
      rand.pop_transform()

   def done(self):
      return self.mem.done()

   def _rotate(self, value, count):
      bits = self.machine.addr_bits - self.machine.word_bits
      mask = (1 << bits) - 1
      shift_part = value >> self.machine.word_bits
      word_part = value & self.machine.word_mask
      if count >= 0:
         count2 = bits - count
         shifted = ((shift_part << count) & mask) | (shift_part >> count2)
      else:
         count2 = bits + count
         shifted = ((shift_part << count2) & mask) | (shift_part >> -count)
      return (shifted << self.machine.word_bits) | word_part

   def process(self, write, addr, size):
      addr = self._rotate(addr, self.shift)
      return self.bank.process(write, addr, size)

   def forward(self, index, write, addr, size):
      addr = self._rotate(addr, -self.shift)
      return self.mem.process(write, addr, size)

