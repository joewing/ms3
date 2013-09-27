
from Memory import Memory
from Machine import get_address, get_size, get_cycles

class RAM(Memory):

   def __init__(self, machine, latency = 100):
      self.latency = latency
      self.word_size = machine.word_size
      self.machine = machine

   def __str__(self):
      return '(ram (latency ' + str(self.latency) + '))'

   def reset(self):
      pass

   def done(self):
      return 0

   def process(self, access):
      addr = get_address(access)
      size = get_size(access)
      assert(size > 0)
      offset = addr % self.word_size
      count = (size + self.word_size + offset - 1) / self.word_size
      t = count * self.latency + get_cycles(access)
      self.machine.time += t
      return 0

