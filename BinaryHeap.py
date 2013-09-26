
class BinaryHeap:
   def __init__(self):
      self.heap = [0]
      self.size = 0

   def push(self, key, value):
      self.size += 1
      if len(self.heap) <= self.size:
         self.heap.append((key, value))
      else:
         self.heap[self.size] = (key, value)
      i = self.size
      while i > 1:
         ni = i / 2
         if self.heap[i][0] < self.heap[ni][0]:
            self.swap(i, ni)
            i = ni
         else:
            break

   def key(self):
      return self.heap[1][0]

   def value(self):
      return self.heap[1][1]

   def empty(self):
      return self.size == 0

   def swap(self, a, b):
      self.heap[a], self.heap[b] = self.heap[b], self.heap[a]

   def pop(self):
      self.heap[1] = self.heap[self.size]
      self.size -= 1
      i = 1
      while True:
         left = i * 2
         right = left + 1
         if right <= self.size:
            if self.heap[left][0] < self.heap[right][0]:
               if self.heap[i][0] > self.heap[left][0]:
                  self.swap(i, left)
                  i = left
            elif self.heap[i][0] > self.heap[right][0]:
               self.swap(i, right)
               i = right
         elif left <= self.size and self.heap[i][0] > self.heap[left][0]:
            self.swap(i, left)
            i = left
         else:
            break

   def show(self):
      print("HEAP:")
      for i in range(1, self.size + 1):
         k = self.heap[i][0]
         v = self.heap[i][1]
         print("  " + str(k) + " -> " + str(v))

