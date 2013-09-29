
class Benchmark:
   """Base clase for benchmarks.
      A benchmark is a kernel used to generate an address trace.
   """

   word_size = 4
   offset = 0

   def __init__(self, word_size):
      self.word_size = word_size

   def read(self, addr):
      """Generate a read."""
      addr *= self.word_size
      addr += self.offset
      return False, addr, self.word_size

   def write(self, addr):
      """Generate a write."""
      addr *= self.word_size
      addr += self.offset
      return True, addr, self.word_size

   def reset(self, offset):
      """Prepare the benchmark to be run and set the address offset."""
      self.offset = offset

   def run(self):
      """Run the benchmark.
         Note that the results of a benchmark should be deterministic.
         This function should use 'yield' memory accesses.
      """
      assert(False)


