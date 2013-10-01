
from PriorityQueue import PriorityQueue

class AccessType:
   READ = 0       # Read from memory
   WRITE = 1      # Write to memory
   IDLE = 2       # Idle
   CONSUME = 3    # Consume a value from an input port
   PRODUCE = 4    # Produce a value on an output port
   END = 5        # Produce a value indicating the end of a stream

class Process:

   def __init__(self, dist, benchmark):
      self.time = 0
      self.mem = None
      self.dist = dist
      self.benchmark = benchmark
      self.machine = None
      self.waiting = -1

   def get_cost(self):
      return self.mem.get_total_cost()

   def reset(self, machine, mem, offset):
      self.mem = mem
      self.machine = machine
      self.benchmark.reset(offset)
      self.generator = self.benchmark.run()
      self.mem.reset(machine)

   def done(self):
      return self.mem.done()

   def step(self, first):
      self.time = 0
      if self.waiting >= 0:
         if self.machine.consume(self.waiting):
            self.waiting = -1
         return True
      try:
         at, addr, size = next(self.generator)
         if first:
            self.dist.insert_range(addr, size)
         if at == AccessType.READ:
            self.time = self.mem.process(False, addr, size)
         elif at == AccessType.WRITE:
            self.time = self.mem.process(True, addr, size)
         elif at == AccessType.IDLE:
            self.time = addr
         elif at == AccessType.PRODUCE:
            self.machine.produce(addr)
         elif at == AccessType.CONSUME:
            if not self.machine.consume(addr):
               waiting = addr
         elif at == AccessType.END:
            self.machine.end(addr)
         else:
            assert(False)
         return True
      except StopIteration:
         return False

class ProcessList:

   def __init__(self, machine, processes):
      self.heap = PriorityQueue()
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
         self.machine.time = max(self.machine.time, self.heap.key())
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

