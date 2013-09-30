
from PriorityQueue import PriorityQueue

class Process:

   time = 0
   mem = None

   def __init__(self, dist, benchmark):
      self.dist = dist
      self.benchmark = benchmark

   def get_cost(self):
      return self.mem.get_total_cost()

   def reset(self, machine, mem, offset):
      self.mem = mem
      self.benchmark.reset(offset)
      self.generator = self.benchmark.run()
      self.mem.reset(machine)

   def done(self):
      return self.mem.done()

   def step(self, first):
      try:
         write, addr, size = next(self.generator)
         if first:
            self.dist.insert_range(addr, size)
         self.time = self.mem.process(write, addr, size)
         return True
      except StopIteration:
         return False

class ProcessList:

   heap = PriorityQueue()

   def __init__(self, machine, processes):
      self.machine = machine
      self.processes = processes

   def reset(self, ml):
      self.machine.time = 0
      for i in range(len(self.processes)):
         p = self.processes[i]
         p.reset(self.machine, ml.memories[i], self.machine.flip(i))
         self.heap.push(0, p)

   def run(self, ml, first = False):
      print(ml.get_name())
      self.reset(ml)
      while not self.heap.empty():
         k = self.heap.key()
         if self.machine.time < k:
            self.machine.time = k
         p = self.heap.value()
         self.heap.pop()
         if p.step(first):
            self.heap.push(self.machine.time + p.time, p)
      for p in self.processes:
         t = p.done()
         if t > self.machine.time:
            self.machine.time = t
      print("Time: " + str(self.machine.time))
      print("Cost: " + str(ml.get_cost()))
      return self.machine.time

