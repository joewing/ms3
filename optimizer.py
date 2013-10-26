
import random
import StringIO

import database
import lex
import memory
import memory.cache as cache
import memory.offset as offset
import memory.prefetch as prefetch
import memory.spm as spm
import memory.shift as shift
import memory.split as split
import memory.xor as xor

class Optimizer:

   steps = 0
   evaluations = 0
   threshold = 1024
   age = 0
   best_name = ''
   best_value = -1
   best_cost = 0
   last = None
   last_value = 0
   current = None

   constructors = [
      cache.random_cache,
      offset.random_offset,
      shift.random_shift,
      split.random_split,
      spm.random_spm,
      xor.random_xor
   ]

   def __init__(self, machine, ml, seed,
                permute_only = False,
                use_prefetch = False):
      self.current = ml
      self.machine = machine
      self.rand = random.Random(seed)
      self.permute_only = permute_only
      self.current.reset(machine)
      if use_prefetch:
         self.constructors.append(prefetch.random_prefetch)

   def _load_memory_list(self, s):
      if s is None:
         return s
      else:
         dists = self.current.distributions
         lexer = lex.Lexer(StringIO.StringIO(s))
         ml = memory.parse_memory_list(lexer, dists)
         ml.reset(self.machine)
         return ml

   def load(self):
      """Load state from a database."""
      db = database.get_instance()
      self.steps = db.get_value('steps', 0)
      self.evaluations = db.get_value('evaluations', 0)
      self.threshold = db.get_value('threshold', 1024)
      self.age = db.get_value('age', 0)
      self.best_name = db.get_value('best_name', '')
      self.best_value = db.get_value('best_value', -1)
      self.best_cost = db.get_value('best_cost', 0)
      self.last = self._load_memory_list(db.get_value('last'))
      self.last_value = db.get_value('last_value', 0)
      temp = db.get_value('current')
      if temp is not None:
         self.current = self._load_memory_list(temp)
      use_prefetch = db.get_value('use_prefetch')
      if use_prefetch and prefetch.random_prefetch not in self.constructors:
         self.constructors.append(prefetch.random_prefetch)
      return self.current.simplified()

   def _save(self):
      """Save the current state to a database."""
      db = database.get_instance()
      db.set_value('steps', self.steps)
      db.set_value('evaluations', self.evaluations)
      db.set_value('threshold', self.threshold)
      db.set_value('age', self.age)
      db.set_value('best_name', self.best_name)
      db.set_value('best_value', self.best_value)
      db.set_value('best_cost', self.best_cost)
      if self.last is not None:
         db.set_value('last', str(self.last))
      db.set_value('last_value', self.last_value)
      if self.current is not None:
         db.set_value('current', str(self.current))
      use_prefetch = prefetch.random_prefetch in self.constructors
      db.set_value('use_prefetch', use_prefetch)
      for i in range(len(self.current.distributions)):
         dist = self.current.distributions[i]
         dist.save(i)

   def create_memory(self, dist, nxt, cost, in_bank):
      index = self.rand.randint(0, len(self.constructors) - 1)
      constructor = self.constructors[index]
      result = constructor(self.machine, nxt, dist, cost)
      if result is not None:
         result.reset(self.machine)
         return result
      else:
         return nxt

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
         if n is None: return mem
         for b in mem.get_banks():
            if not isinstance(b, memory.Join):
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
      max_cost = self.machine.max_cost - self.current.get_cost()
      stat = False
      while not stat:

         # Select a memory to modify.
         # Note that we do not attempt to modify a memory subsystem
         # unless it is actually used, which we determine using
         # Distribution.is_empty.
         while True:
            mindex = self.rand.randint(0, len(self.current) - 1)
            mem = self.current.memories[mindex]
            dist = self.current.distributions[mindex]
            if not dist.is_empty(): break
         count = mem.count()

         # Select an action to perform.
         action = self.rand.randint(0, 7)
         if action == 0:   # Insert
            for i in range(100):
               before = str(mem)
               index = self.rand.randint(0, count - 1)
               temp = self.insert(dist, mem, index, max_cost)
               stat = temp is not None and str(temp) != before
               if stat:
                  self.current.memories[mindex] = temp
                  break
         elif action <= 2 and count > 1: # Remove
            for i in range(100):
               before = str(mem)
               index = self.rand.randint(0, count - 1)
               temp = self.remove(dist, mem, index)
               stat = temp is not None and str(temp) != before
               if stat:
                  self.current.memories[mindex] = temp
                  break
         else: # Permute
            index = self.rand.randint(0, count - 1)
            stat = self.permute(dist, mem, index, max_cost)

   def update_best(self, simplified, time):
      """Update and display the best memory found so far."""
      cost = simplified.get_cost()
      name = str(simplified)
      if self.best_value == -1 or time < self.best_value or \
         (time == self.best_value and cost < self.best_cost) or \
         (time == self.best_value and cost == self.best_cost and \
            len(name) < len(self.best_name)):
         self.best_name = name
         self.best_cost = cost
         self.best_value = time
      print("Best Memory: " + str(self.best_name))
      print("Best Value:  " + str(self.best_value))
      print("Best Cost:   " + str(self.best_cost))

   def generate_next(self, time):
      """Generate the next memory to try."""
      db = database.get_instance()
      while True:
         self.steps += 1
         if self.last is None:
            self.last = self.current.clone()
         else:
            diff = time - self.last_value
            if diff <= self.threshold:
               # Keep the current memory.
               self.last_value = time
               self.last = self.current.clone()
               self.threshold -= (self.threshold + 1023) // 1024
               self.age = 0
            else:
               # Revert to the last memory.
               self.current = self.last.clone()
               self.threshold += 1 + (self.age * self.threshold) // 2048
               self.age += 1
            before = self.current.clone()
            while True:
               self.modify()
               simplified = self.current.simplified()
               ml = simplified.get_max_path_length()
               if ml <= self.machine.max_path_length: break
               self.current = before.clone()
            time = db.get_result(str(simplified))
            if time is None:
               return self.current
            else:
               self.age += 1

   def optimize(self, time):
      """This function is to be called after each evaluation.
         It returns the next memory list to evaluate, None when complete.
      """
      self.evaluations += 1

      # Cache the simplified memory.
      db = database.get_instance()
      simplified = self.current.simplified()
      db.add_result(str(simplified), time)

      # Track the best.
      self.update_best(simplified, time)

      # Generate the next subsystem and display stats.
      result = self.generate_next(time)
      temp  = "Iteration: " + str(self.evaluations + 1)
      temp += " (steps " + str(self.steps + 1)
      temp += ", threshold " + str(self.threshold)
      temp += ", age " + str(self.age)
      temp += ")"
      print(temp)
      self._save()
      return result.simplified()

