
import copy

class Memory:
   """The abstract base class for all memory components."""

   def clone(self):
      """Create a deep copy of this memory."""
      return copy.deepcopy(self)

   def get_next(self):
      """Get the next memory component."""
      return None

   def get_banks(self):
      """Get memory subcomponents."""
      return []

   def set_next(self, n):
      """Set the next memory."""
      assert(False)

   def set_bank(self, i, c):
      """Update a memory sub-component."""
      assert(False)

   def count(self):
      """Count the total number of components that make up this memory."""
      counts = map(lambda m: m.count(), self.get_banks())
      result = reduce(lambda a, b: a + b, counts, 1)
      n = self.get_next()
      if n != None: result += n.count()
      return result

   def get_cost(self):
      """Get the cost of this memory component (shallow)."""
      return 0

   def get_total_cost(self):
      """Get the total cost of the memory component and its children."""
      components = filter(lambda m: m != None, self.get_banks())
      costs = map(lambda m: m.get_total_cost(), components)
      result = reduce(lambda a, b: a + b, costs, self.get_cost())
      if self.get_next() != None:
         result += self.get_next().get_total_cost()
      return result

   def push_transform(self, index, rand):
      """Push any address transforms or limits for bank index.
         index=-1 is used for the next memory.
      """
      pass

   def pop_transform(self, rand):
      """Pop any address transforms or limits."""
      pass

   def permute(self, rand, max_cost):
      """Permute a memory component.
         This function will permute the memory component without
         exceeded max_cost for that component.  This returns
         True if the component was modified and False otherwise.
      """
      return False

   def reset(self):
      """Reset the memory for a new run.
         This is called before every trace execution.
      """
      for b in self.get_banks():
         b.reset()
      if self.get_next() != None:
         self.get_next().reset()

   def process(self, write, addr, size):
      """Process a memory access operation.
         This function will return the number of cycles spent
         in the memory subsystem (excluding the main memory).
      """
      return 0

   def done(self):
      """Finish a trace execution.
         This will return the number of cycles to add to the
         result for the memory subsystem.
      """
      return 0

