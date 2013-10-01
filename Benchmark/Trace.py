
import re
from Benchmark import Benchmark

class Trace(Benchmark):
   """Benchmark to replay an address trace."""

   def __init__(self, file_name):
      Benchmark.__init__(self)
      self.file_name = file_name
      self.expr = re.compile(r'([RW])([0-9a-fA-F]+):([0-9a-fA-F]+)')

   def run(self):
      with open(self.file_name, 'r') as f:
         for line in f:
            for m in self.expr.finditer(line):
               write = m.group(1) == 'W'
               addr = int(m.group(2), 16)
               size = int(m.group(3), 16)
               yield write, addr, size

