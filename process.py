
import sys
import priorityqueue

class AccessType:
   """Enumeration of possible memory access types."""
   READ = 0       # Read from memory
   WRITE = 1      # Write to memory
   IDLE = 2       # Idle
   CONSUME = 3    # Consume a value from an input port
   PRODUCE = 4    # Produce a value on an output port
   END = 5        # Produce a value indicating the end of a stream

class Process:
   """A class to represent processes that perform memory accesses."""

   def __init__(self, dist, benchmark):
      """Initialize a process.
         dist is the Distribution to use.
         benchmark is the Benchmark to generate the memory accesses.
      """
      self.mem = None
      self.dist = dist
      self.benchmark = benchmark
      self.machine = None
      self.waiting = -1

   def reset(self, machine, mem, offset):
      """Reset this process for the next simulation.
         machine is the MachineType to use.
         mem is the memory subsystem.
         offset is the address offset for this process.
      """
      self.mem = mem
      self.machine = machine
      self.benchmark.reset(offset)
      self.generator = self.benchmark.run()
      self.mem.reset(machine)

   def done(self):
      """Get the final simulation time for this process.
         Note that this returns an absolute time.
      """
      return self.mem.done()

   def step(self, first):
      """Peform the next event.
         first is set to True on the first execution.
         This returns the amount of time used (a delta).
         If the result is negative, then this process is waiting
         to consume an input.
      """

      # Don't continue if we are waiting to consume an input.
      if self.waiting >= 0:
         if self.machine.consume(self.waiting):
            self.waiting = -1
            return -1

      # Get the next access and register it with the distribution.
      at, addr, size = next(self.generator)
      if first:
         if addr > self.machine.addr_mask:
            print("ERROR: address out of range")
            sys.exit(-1)
         self.dist.insert_range(addr, size)

      # Perform the access.
      if at == AccessType.READ:
         return self.mem.process(0, False, addr, size)
      elif at == AccessType.WRITE:
         return self.mem.process(0, True, addr, size)
      elif at == AccessType.IDLE:
         return addr
      elif at == AccessType.PRODUCE:
         self.machine.produce(addr)
      elif at == AccessType.CONSUME:
         if not self.machine.consume(addr):
            self.waiting = addr
      elif at == AccessType.END:
         self.machine.end(addr)
      else:
         assert(False)
      return 0

class ProcessList:
   """Class to schedule a list of processes on a machine."""

   def __init__(self, machine, processes):
      """Initialize the process list.
         machine is the MachineType instance to use.
         processes is a list of Process objects.
      """
      self.heap = priorityqueue.PriorityQueue()
      self.machine = machine
      self.processes = processes
      self.first = True

   def run(self, ml):
      """Run a simulation.
         ml is the MemoryList describing the memories to use.
      """
      print(ml.get_name())

      # Reset to prepare for the simulation.
      self.machine.reset()
      for i in range(len(self.processes)):
         p = self.processes[i]
         p.reset(self.machine, ml.memories[i], self.machine.flip(i))
         self.heap.push(0, p)

      # Run the simulation until there are no more events to process.
      while not self.heap.empty():
         self.machine.time = max(self.machine.time, self.heap.key())
         p = self.heap.value()
         self.heap.pop()
         try:
            delta = p.step(self.first)
            if delta >= 0:
               self.heap.push(self.machine.time + delta, p)
            elif not self.heap.empty():
               self.heap.push(self.machine.time + self.heap.key(), p)
            else:
               print("ERROR: wait to consume without active producers")
               sys.exit(-1)
         except StopIteration:
            pass

      # Take into account any leftover time.
      for p in self.processes:
         t = p.done()
         if t > self.machine.time:
            self.machine.time = t

      # No longer the first execution.
      self.first = False

      # Display the results and return.
      print("Time: " + str(self.machine.time))
      print("Cost: " + str(ml.get_cost()))
      return self.machine.time

