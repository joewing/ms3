
from Benchmark import Benchmark

class MM(Benchmark):
   """Benchmark to simulate matrix-matrix multiplication."""

   def __init__(self, machine, size, iterations = 1):
      self.size = size
      self.iterations = iterations

   def run(self):
      wsize = 4
      msize = self.size * self.size * wsize
      srca = 0 * msize
      srcb = 1 * msize
      dest = 2 * msize
      for i in range(self.iterations):
         for a in range(self.size):
            for b in range(self.size):
               yield self.write(dest + a * self.size + b, wsize)
               for c in range(self.size):
                  yield self.read(srca + b * self.size + c, wsize)
                  yield self.read(srcb + c * self.size + a, wsize)
                  yield self.write(dest + a * self.size + b, wsize)


