
import base
import parser

class PCA(base.Benchmark):

   def __init__(self, size, count, iterations, input_port, output_port):
      base.Benchmark.__init__(self)
      self.size = size
      self.count = count
      self.iterations = iterations
      self.input_port = input_port
      self.output_port = output_port

   def run(self):
      r_offset = 0
      s_offset = r_offset + self.size
      x_offset = s_offset + self.size

      for it in range(self.iterations):

         if self.input_port >= 0:
            for i in range(self.size * self.size):
               yield self.consume(self.input_port)
               yield self.write(x_offset + i)

         for c in range(self.count):

            # s = 0
            for i in range(self.size):
               yield self.write(s_offset + i)

            # for each row in X
            for r in range(self.size):

               # s = s + (x * r) * x
               for i in range(self.size):
                  yield self.read(r_offset + i)
                  yield self.read(x_offset + r * self.size + i)
                  yield self.read(s_offset + i)
                  yield self.write(s_offset + i)

               # r = s / |s|
               for i in range(self.size):
                  yield self.read(s_offset + i)
                  yield self.write(r_offset + i)

            if self.output_port >= 0:
               for i in range(self.size * self.size):
                  yield self.read(x_offset + i)
                  yield self.produce(self.output_port)

def _create_pca(args):
   size = parser.get_argument(args, 'size', 64)
   count = parser.get_argument(args, 'count', 16)
   iterations = parser.get_argument(args, 'iterations', 1)
   input_port = parser.get_argument(args, 'input_port', -1)
   output_port = parser.get_argument(args, 'output_port', -1)
   return PCA(size, count, iterations, input_port, output_port)
base.constructors['pca'] = _create_pca
