
import base
import parser

class RAM(base.Memory):

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

   def process(self, start, write, addr, size):
      assert(size > 0)
      word_size = self.machine.word_size
      offset = addr % word_size
      count = (size + word_size + offset - 1) // word_size
      if self.burst == 0:
         return start + count * self.latency
      else:
         return start + self.latency + self.burst * (count - 1)

def _create_ram(lexer, args):
   latency = parser.get_argument(lexer, args, 'latency', 100)
   return RAM(latency)
base.constructors['ram'] = _create_ram
