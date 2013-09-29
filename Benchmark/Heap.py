
import random
from Benchmark import Benchmark

class Heap(Benchmark):
   """Benchmark to simulate operations on a binary heap."""

   def __init__(self, seed, size):
      self.seed = seed
      self.size = size

   def _insert(self, heap, value):
      heap[0] += 1
      size = heap[0]
      index = size
      yield self.write(0)
      while index > 1:
         parent = index >> 1
         yield self.read(parent)
         if value >= heap[parent]:
            break
         yield self.write(index)
         heap[index] = heap[parent]
         index = parent
      heap[index] = value
      yield self.write(index)

   def _remove(self, heap):
      size = heap[0]
      heap[0] -= 1
      yield self.read(size)
      displaced = heap[size]
      yield self.read(1)
      index = 1
      while True:
         left = index << 1
         right = left + 1
         if right < size:
            yield self.read(left)
            yield self.read(right)
            if heap[right] > heap[left] and displaced > heap[left]:
               yield self.write(index)
               heap[index] = heap[left]
               index = left
            elif heap[left] >= heap[right] and displaced > heap[right]:
               yield self.write(index)
               heap[index] = heap[right]
               index = right
            else:
               break
         elif left < size:
            yield self.read(left)
            if displaced > heap[left]:
               yield self.write(index)
               heap[index] = heap[left]
               index = left
            else:
               break
         else:
            break
      heap[index] = displaced
      yield self.write(index)

   def run(self):
      rand = random.Random(self.seed)
      heap = [0] * (self.size + 1)
      for i in range(self.size):
         value = rand.randint(0, 1 << 30)
         for a in self._insert(heap, value):
            yield a
      for i in range(self.size):
         for a in self._remove(heap):
            yield a

