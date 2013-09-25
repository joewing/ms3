
from Machine import create_access

class Benchmark:

   def read(self, addr, size):
      return create_access(is_write=False, address=addr, size=size)

   def write(self, addr, size):
      return create_access(is_write=True, address=addr, size=size)

   def run(self):
      assert(False)


