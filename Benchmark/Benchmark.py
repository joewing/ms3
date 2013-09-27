
from Machine import create_access

class Benchmark:
   """Base clase for benchmarks.
      A benchmark is a kernel used to generate an address trace.
   """

   def read(self, addr, size):
      """Generate a read."""
      return create_access(is_write = False, address = addr, size = size)

   def write(self, addr, size):
      """Generate a write."""
      return create_access(is_write = True, address = addr, size = size)

   def run(self):
      """Run the benchmark.
         Note that the results of a benchmark should be deterministic.
         This function should use 'yield' memory accesses.
      """
      assert(False)


