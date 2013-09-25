
class MockFile:

   def __init__(self, value):
      self.value = value
      self.index = 0

   def read(self, size):
      if self.index + size >= len(self.value):
         size = len(self.value) - self.index + 1
      result = self.value[self.index:self.index + size]
      self.index += size
      return result

