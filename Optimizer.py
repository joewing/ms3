
import random
import Memory.Cache as Cache
import Memory.Offset as Offset
import Memory.SPM as SPM
import Memory.Join as Join
import Memory.Shift as Shift
import Memory.Split as Split

class Optimizer:

   cache = dict()
   steps = 0
   evaluations = 0
   iterations = 0
   threshold = 1024
   age = 0
   best_name = ''
   best_value = -1
   best_cost = 0
   last = None
   last_value = 0

   constructors = [
      Offset.random_offset,
      SPM.random_spm,
      Cache.random_cache,
      Shift.random_shift,
      Split.random_split
   ]

   def __init__(self, machine, pl,
                max_cost = 1e6,
                iterations = 1000,
                seed = 7,
                permute_only = False):
      self.pl = pl
      self.machine = machine
      self.max_cost = max_cost
      self.max_iterations = iterations
      self.rand = random.Random(seed)
      self.permute_only = permute_only

   def create_memory(self, dist, nxt, cost, in_bank):
      index = self.rand.randint(0, len(self.constructors) - 1)
      constructor = self.constructors[index]
      return constructor(self.machine, nxt, dist, cost)

   def permute(self, dist, mem, index, max_cost):
      """Permute a specific memory component.
         Returns True if successful.
      """
      assert(index >= 0)
      if index == 0:
         mc = max_cost + mem.get_cost()
         return mem.permute(dist, mc)
      n = mem.get_next()
      nc = n.count()
      if index <= nc:
         mem.push_transform(-1, dist)
         result = self.permute(dist, n, index - 1, max_cost)
         mem.pop_transform(dist)
         return result
      t = nc + 1
      banks = mem.get_banks()
      for i in range(len(banks)):
         c = banks[i].count()
         if index < t + c:
            mem.push_transform(i, dist)
            result = self.permute(dist, banks[i], index - t, max_cost)
            mem.pop_transform(dist)
            return result
         t += c
      assert(False)

   def insert(self, dist, mem, index, max_cost):
      """Insert a memory component before index.
         Returns the updated memory.
      """
      assert(index >= 0)
      if index == 0:
         return self.create_memory(dist, mem, max_cost, False)
      n = mem.get_next()
      nc = n.count()
      if index <= nc:
         mem.push_transform(-1, dist)
         mem.set_next(self.insert(dist, n, index - 1, max_cost))
         mem.pop_transform(dist)
         return mem
      banks = mem.get_banks()
      t = nc + 1
      for i in range(len(banks)):
         c = banks[i].count()
         if index < t + c:
            mem.push_transform(i, dist)
            mem.set_bank(i, self.insert(dist, banks[i], index - t, max_cost))
            mem.pop_transform(dist)
            return mem
         t += c
      assert(False)

   def remove(self, dist, mem, index):
      """ Remove a memory component at index.
          Returns the updated memory.
      """
      assert(index >= 0)
      n = mem.get_next()
      if index == 0:
         if n == None: return mem
         for b in mem.get_banks():
            if not isinstance(b, Join.Join):
               return mem
         return n
      nc = n.count()
      if index <= nc:
         mem.push_transform(-1, dist)
         mem.set_next(self.remove(dist, n, index - 1))
         mem.pop_transform(dist)
         return mem
      t = nc + 1
      banks = mem.get_banks()
      for i in range(len(banks)):
         c = banks[i].count()
         if index < t + c:
            mem.push_transform(i, dist)
            updated = self.remove(dist, banks[i], index - t)
            mem.set_bank(i, updated)
            mem.pop_transform(dist)
            return mem
         t += c
      assert(False)

   def modify(self):
      """Modify the memory subsystem."""

      # Loop until we successfully modify the memory subsystem.
      total_cost = self.pl.get_cost()
      max_cost = self.max_cost - total_cost
      stat = False
      while not stat:

         # Select a process to modify.
         pindex = self.rand.randint(0, len(self.pl.processes) - 1)
         p = self.pl.processes[pindex]
         count = p.mem.count()

         # Select an action to perform.
         action = self.rand.randint(0, 7)
         if action == 0:   # Insert
            for i in range(100):
               before = str(p.mem)
               index = self.rand.randint(0, count - 1)
               temp = self.insert(p.dist, p.mem, index, max_cost)
               stat = temp != None and str(temp) != before
               if stat:
                  p.mem = temp
                  break
         elif action <= 2 and count > 1: # Remove
            for i in range(100):
               before = str(p.mem)
               index = self.rand.randint(0, count - 1)
               temp = self.remove(p.dist, p.mem, index)
               stat = temp != None and str(temp) != before
               if stat:
                  p.mem = temp
                  break
         else: # Permute
            index = self.rand.randint(0, count - 1)
            stat = self.permute(p.dist, p.mem, index, max_cost)

   def update_best(self, time):
      """Update and display the best memory found so far."""
      cost = self.pl.get_cost()
      name = self.pl.get_name()
      if self.best_value == -1 or time < self.best_value or \
         (time == self.best_value and cost < self.best_cost) or \
         (time == self.best_value and cost == self.best_cost and \
            len(name) < len(self.best_name)):
         self.best_name = name
         self.best_cost = cost
         self.best_value = time
         self.iterations = 0
      print("Best Memory: " + str(self.best_name))
      print("Best Value:  " + str(self.best_value))
      print("Best Cost:   " + str(self.best_cost))

   def generate_next(self, time):
      """Generate the next memory to try."""
      self.iterations += 1
      self.steps += 1
      if self.iterations > self.max_iterations:
         return False
      if self.last == None:
         self.last = self.pl.clone()
      else:
         diff = time - self.last_value
         if diff <= self.threshold:
            # Keep the current memory.
            self.last_value = time
            self.last = self.pl.clone()
            self.threshold -= (self.threshold + 1023) / 1024
            self.age = 0
         else:
            # Revert to the last memory.
            self.pl.mem = self.last.clone()
            self.threshold += 1 + (self.age * self.threshold) / 2048
            self.age += 1
         self.modify()
         new_name = self.pl.get_name()
         if new_name in self.cache:
            return self.generate_next(self.cache[new_name])
      return True
      

   def optimize(self, time):
      """This function is to be called after each evaluation.
         It returns True if the trace should be re-evaluated, False otherwise.
      """
      self.evaluations += 1
      self.update_best(time)
      self.cache[self.pl.get_name()] = time
      result = self.generate_next(time)
      if result:
         temp  = "Iteration: " + str(self.iterations)
         temp += " (evaluation " + str(self.evaluations + 1)
         temp += ", steps " + str(self.steps + 1)
         temp += ", threshold " + str(self.threshold)
         temp += ", age " + str(self.age)
         temp += ")"
         print(temp)
      return result

