
from Machine import create_access

class Benchmark:
   """Base clase for benchmarks.
      A benchmark is a kernel used to generate an address trace.
   """

   offset = 0

   def read(self, addr, size):
      """Generate a read."""
      addr += self.offset
      return create_access(is_write = False, address = addr, size = size)

   def write(self, addr, size):
      """Generate a write."""
      addr += self.offset
      return create_access(is_write = True, address = addr, size = size)

   def reset(self, offset):
      """Prepare the benchmark to be run and set the address offset."""
      self.offset = offset

   def run(self):
      """Run the benchmark.
         Note that the results of a benchmark should be deterministic.
         This function should use 'yield' memory accesses.
      """
      assert(False)


