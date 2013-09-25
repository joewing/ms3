
import BinaryHeap

class Process:

   def __init__(self, mem, benchmark):
      self.time = 0
      self.mem = mem
      self.generator = benchmark.run()

   def step(self):
      try:
         access = next(self.generator)
         self.time = self.mem.process(access)
         return True
      except StopIteration:
         return False

class ProcessList:

   def __init__(self, machine):
      self.processes = BinaryHeap.BinaryHeap()
      self.machine = machine

   def insert(self, p):
      self.processes.push(self.machine.time, p)

   def run(self):
      while not self.processes.empty():
         k = self.processes.key()
         if self.machine.time < k:
            self.machine.time = k
         p = self.processes.value()
         self.processes.pop()
         if p.step():
            self.processes.push(self.machine.time + p.time, p)

