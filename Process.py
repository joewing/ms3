
import BinaryHeap

class Process:

   def __init__(self, mem, benchmark):
      self.time = 0
      self.mem = mem
      self.generator = benchmark.run()

   def get_cost(self):
      return self.mem.get_total_cost()

   def reset(self):
      self.mem.reset()

   def done(self):
      return self.mem.done()

   def step(self):
      try:
         access = next(self.generator)
         self.time = self.mem.process(access)
         return True
      except StopIteration:
         return False

class ProcessList:

   def __init__(self, machine):
      self.processes = []
      self.heap = BinaryHeap.BinaryHeap()
      self.machine = machine

   def insert(self, p):
      self.processes.append(p)

   def get_cost(self):
      costs = map(lambda m: m.get_cost(), self.processes)
      return reduce(lambda x, y: x + y, costs, 0);

   def run(self):
      self.machine.time = 0
      for p in self.processes:
         p.reset()
         self.heap.push(0, p)
      while not self.heap.empty():
         k = self.heap.key()
         if self.machine.time < k:
            self.machine.time = k
         p = self.heap.value()
         self.heap.pop()
         if p.step():
            self.heap.push(self.machine.time + p.time, p)
      for p in self.processes:
         t = p.done()
         if t > self.machine.time:
            self.machine.time = t
      return self.machine.time

