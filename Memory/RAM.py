
from Memory import Memory

class RAM(Memory):

   def __init__(self, latency = 100):
      self.latency = latency

   def __str__(self):
      return '(ram (latency ' + str(self.latency) + '))'

   def done(self):
      return 0

   def process(self, write, addr, size):
      assert(size > 0)
      word_size = self.machine.word_size
      offset = addr % word_size
      count = (size + word_size + offset - 1) / word_size
      t = count * self.latency
      self.machine.time += t
      return 0

