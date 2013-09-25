
from Memory import Memory
from Machine import *

class RAM(Memory):

   def __init__(self, machine, latency=100):
      self.latency = latency
      self.word_size = machine.word_size
      self.machine = machine

   def set_port(self, port): pass

   def process(self, access):
      addr = get_address(access)
      size = get_size(access)
      assert(size > 0)
      offset = addr % self.word_size
      count = (size + self.word_size + offset - 1) / self.word_size
      t = count * self.latency + get_cycles(access)
      self.machine.time += t
      return 0

