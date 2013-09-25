

from Memory.Memory import Memory

class MockMemory(Memory):

   def __init__(self, *components):
      self.components = list(components)

   def __str__(self):
      result = "(mock "
      for c in self.components:
         result += str(c)
      result += ")"
      return result

   def get_components(self):
      return self.components

   def set_component(self, i, c):
      assert(i < len(self.components))
      self.components[i] = c

