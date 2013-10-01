
import random
from Benchmark import Benchmark

class QSort(Benchmark):
   """Benchmark to simulate quick sort."""

   def __init__(self, seed, size, input_port, output_port):
      Benchmark.__init__(self)
      self.seed = seed
      self.size = size
      self.input_port = input_port
      self.output_port = output_port

   def _sort(self, array, left, right):
      yield self.read(left)
      pivot = array[left]
      a, b = left, right
      while True:
         while a <= right:
            yield self.read(a)
            if array[a] >= pivot: break
            a += 1
         while b >= left:
            yield self.read(b)
            if array[b] <= pivot: break
            b -= 1
         if a > b: break
         array[a], array[b] = array[b], array[a]
         yield self.write(a)
         yield self.write(b)
         a += 1
         b -= 1
      if a - 1 > left:
         for t in self._sort(array, left, a - 1):
            yield t
      if right > a:
         for t in self._sort(array, a, right):
            yield t

   def run(self):
      rand = random.Random(self.seed)
      array = []
      for i in range(self.size):
         array.append(rand.randint(0, 1 << 30))
         yield self.consume(self.input_port)
      for a in self._sort(array, 0, self.size - 1):
         yield a
      for i in range(self.size):
         yield self.produce(self.output_port)

