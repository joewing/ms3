
import BinaryHeap
import copy

class Process:

   time = 0

   def __init__(self, mem, benchmark):
      self.mem = mem
      self.benchmark = benchmark

   def get_cost(self):
      return self.mem.get_total_cost()

   def reset(self):
      self.generator = self.benchmark.run()
      self.mem.reset()

   def done(self):
      return self.mem.done()

   def step(self, rand):
      try:
         access = next(self.generator)
         if rand != None:
            rand.insert_range(access)
         self.time = self.mem.process(access)
         return True
      except StopIteration:
         return False

class ProcessList:

   processes = []
   heap = BinaryHeap.BinaryHeap()

   def __init__(self, machine):
      self.machine = machine

   def clone(self):
      return copy.deepcopy(self)

   def insert(self, p):
      self.processes.append(p)

   def get_cost(self):
      costs = map(lambda m: m.get_cost(), self.processes)
      return reduce(lambda x, y: x + y, costs, 0);

   def get_name(self):
      names = map(lambda p: str(p.mem), self.processes)
      return reduce(lambda a, b: a + ":" + b, names)

   def run(self, rand = None):
      print self.get_name()
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
         if p.step(rand):
            self.heap.push(self.machine.time + p.time, p)
      for p in self.processes:
         t = p.done()
         if t > self.machine.time:
            self.machine.time = t
      print "Time: " + str(self.machine.time)
      return self.machine.time

