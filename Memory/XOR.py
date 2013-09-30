
from Transform import Transform
from Join import Join

def random_xor(machine, nxt, rand, cost):
   value = 1 << rand.randint(0, machine.addr_bits - 1)
   join = Join()
   result = XOR(join, nxt, value)
   join.parent = result
   return result

class XOR(Transform):

   def __init__(self, bank, mem, value):
      Transform.__init__(self, bank, mem)
      self.value = value

   def __str__(self):
      result  = '(xor '
      result += '(value ' + str(self.value) + ')'
      result += '(bank ' + str(self.bank) + ')'
      result += '(memory ' + str(self.mem) + ')'
      result += ')'
      return result

   def is_empty(self):
      return self.value == 0

   def permute(self, rand, max_cost):
      self.value = 1 << rand.randint(0, self.machine.addr_bits - 1)
      return True

   def push_transform(self, index, rand):
      if index == 0:
         rand.push_transform(lambda a: a ^ self.value)
      else:
         rand.push_transform(lambda a: a)

   def pop_transform(self, rand):
      rand.pop_transform();

   def process(self, write, addr, size):
      return self.bank.process(write, addr ^ self.value, size)

   def forward(self, index, write, addr, size):
      assert(index == 0)
      return self.mem.process(write, addr ^ self.value, size)

