
from Container import Container
from Join import Join

class Transform(Container):
   """A memory that transforms the address space for a bank."""

   def __init__(self, bank, mem):
      Container.__init__(self, mem)
      self.bank = bank

   def get_banks(self):
      return [self.bank]

   def set_bank(self, i, b):
      assert(i == 0)
      self.bank = b

   def is_empty(self):
      return False

   def simplify(self):
      self.bank = self.bank.simplify()
      self.mem = self.mem.simplify()
      if isinstance(self.bank, Join):
         return self.mem
      if self.is_empty():
         last, t = None, self.bank
         while (not isinstance(t, Join)) or t.parent != self:
            last, t = t, t.get_next()
         assert(last != None) # We already checked for an empty bank.
         last.set_next(self.mem)
         return self.bank
      return self

   def forward(self, index, write, addr, size):
      """Forward a request from the bank to the following memory."""
      assert(False)

