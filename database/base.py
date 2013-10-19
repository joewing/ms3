
class Database:
   """Database used for saving state."""

   def __init__(self, m):
      """Initialize."""
      self.model = str(m)

   def load(self):
      """Load values from the database."""
      return True

   def save(self):
      """Save values to the database."""
      pass

   def get_results(self, mem):
      """Load a cached result."""
      raise StopIteration

   def add_result(self, mem, value):
      """Insert a result to the database."""
      pass

   def set_value(self, key, value):
      """Set a value for the current state."""
      pass

   def get_value(self, key, default = None):
      """Get a value from the current state."""
      return default

