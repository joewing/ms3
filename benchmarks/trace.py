
import re
import base
import parser
from process import AccessType

class Trace(base.Benchmark):
   """Benchmark to replay an address trace."""

   def __init__(self, file_name):
      base.Benchmark.__init__(self)
      self.file_name = file_name
      self.expr = re.compile(r'([RWMIPCX])([0-9a-fA-F]+):([0-9a-fA-F]+)')

   def run(self):
      with open(self.file_name, 'r') as f:
         for line in f:
            for m in self.expr.finditer(line):
               at = m.group(1)
               addr = int(m.group(2), 16)
               size = int(m.group(3), 16)
               if at == 'R':
                  yield AccessType.READ, addr, size
               elif at == 'W':
                  yield AccessType.WRITE, addr, size
               elif at == 'M':
                  yield AccessType.READ, addr, size
                  yield AccessType.WRITE, addr, size
               elif at == 'I':
                  yield AccessType.IDLE, addr, size
               elif at == 'P':
                  yield AccessType.PRODUCE, addr, size
               elif at == 'C':
                  yield AccessType.CONSUME, addr, size
               elif at == 'X':
                  yield AccessType.END, addr, size

def _create_trace(args):
   file_name = parser.get_argument(args, 'file', 'trace.txt')
   return Trace(file_name)
base.constructors['trace'] = _create_trace
