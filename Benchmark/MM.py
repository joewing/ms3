
from Benchmark import Benchmark

class MM(Benchmark):
   """Benchmark to simulate matrix-matrix multiplication."""

   def __init__(self, size, iterations, input_port, output_port):
      Benchmark.__init__(self)
      self.size = size
      self.iterations = iterations
      self.input_port = input_port
      self.output_port = output_port

   def run(self):
      msize = self.size * self.size
      srca = 0 * msize
      srcb = 1 * msize
      dest = 2 * msize
      for i in range(self.iterations):
         if self.input_port >= 0:
            for a in range(self.size):
               for b in range(self.size):
                  yield self.consume(self.input_port)
                  yield self.write(srca + a * self.size + b)
                  yield self.write(srcb + a * self.size + b)
         for a in range(self.size):
            for b in range(self.size):
               for c in range(self.size):
                  yield self.read(srca + b * self.size + c)
                  yield self.read(srcb + c * self.size + a)
               if self.output_port >= 0:
                  yield self.produce(self.output_port)
               else:
                  yield self.write(dest + a * self.size + b)

