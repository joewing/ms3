
from Benchmark import Benchmark

class Cholesky(Benchmark):
   """Benchmark to simulate Cholesky factorization."""

   def __init__(self, size, input_port, output_port):
      Benchmark.__init__(self)
      self.size = size
      self.input_port = input_port
      self.output_port = output_port

   def run(self):
      for i in range(self.size * self.size):
         yield self.consume(self.input_port)
      for i in range(self.size):
         yield self.read(i * self.size + i)
         for j in range(i):
            yield self.read(i * self.size + j)
         for j in range(i + 1, self.size - 1):
            yield self.read(i * self.size + j)
            for k in range(i):
               yield self.read(j * self.size + k)
               yield self.read(i * self.size + k)
            yield self.write(j * self.size + i)
            yield self.produce(self.output_port)

