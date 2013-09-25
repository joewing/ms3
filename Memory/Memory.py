
class Memory:
   """The abstract base class for all memory components."""

   def get_components(self):
      """Get a list of memory sub-components (shallow)."""
      return []

   def get_cost(self):
      """Get the cost of this memory component (shallow)."""
      return 0

   def get_total_cost(self):
      """Get the total cost of the memory component and its children."""
      costs = map(lambda m: m.get_total_cost(), self.get_components())
      return reduce(lambda a, b: a + b, costs, self.get_cost())

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
      pass

   def process(self, access):
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

