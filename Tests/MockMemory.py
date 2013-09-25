

from Memory import Memory

class MockMemory(Memory):

   def __init__(self, *components):
      self.components = components

   def get_components(self):
      return self.components

