
import random
import base
import parser

class Heap(base.Benchmark):
   """Benchmark to simulate operations on a binary heap."""

   def __init__(self, seed, size, input_port, output_port):
      base.Benchmark.__init__(self)
      self.seed = seed
      self.size = size
      self.input_port = input_port
      self.output_port = output_port

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
         yield self.consume(self.input_port)
         value = rand.randint(0, 1 << 30)
         for a in self._insert(heap, value):
            yield a
      for i in range(self.size):
         for a in self._remove(heap):
            yield a
         yield self.produce(self.output_port)

def _create_heap(lexer, args):
   seed = parser.get_argument(lexer, args, 'seed', 7)
   size = parser.get_argument(lexer, args, 'size', 1024)
   input_port = parser.get_argument(lexer, args, 'input_port', -1)
   output_port = parser.get_argument(lexer, args, 'output_port', -1)
   return Heap(seed, size, input_port, output_port)
base.constructors['heap'] = _create_heap

