
from Memory import Memory
from Join import Join
from  Machine import clone_access, get_address

def random_shift(machine, nxt, rand, cost):
   bits = machine.addr_bits - machine.word_bits - 1
   shift = rand.randint(-bits, bits)
   join = Join(machine)
   result = Shift(machine, join, nxt, shift)
   join.parent = result
   return result

class Shift(Memory):

   def __init__(self, machine, bank, mem, shift = 0):
      self.machine = machine
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

   def process(self, access):
      addr = self._rotate(get_address(access), self.shift)
      updated = clone_access(access, address = addr)
      return self.bank.process(updated)

   def forward(self, index, access):
      addr = self._rotate(get_address(access), -self.shift)
      updated = clone_access(access, address = addr)
      return self.mem.process(updated)

