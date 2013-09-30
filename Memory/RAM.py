
from Memory import Memory

class RAM(Memory):

   def __init__(self, latency = 100, burst = 0):
      self.latency = latency
      self.burst = burst

   def __str__(self):
      result  = '(ram '
      result += '(latency ' + str(self.latency) + ')'
      if self.burst != 0:
         result += '(burst ' + str(self.burst) + ')'
      result += ')'
      return result

   def process(self, write, addr, size):
      assert(size > 0)
      word_size = self.machine.word_size
      offset = addr % word_size
      count = (size + word_size + offset - 1) // word_size
      if self.burst == 0:
         t = count * self.latency
      else:
         t = self.latency + self.burst * (count - 1)
      self.machine.time += t
      return 0

