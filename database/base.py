
import hashlib

class Database:
   """Database used for saving state."""

   def __init__(self, m):
      """Initialize."""
      self.model = str(m)
      self.state = dict()

   def load(self):
      """Load values from the database."""
      return True

   def save(self):
      """Save values to the database."""
      pass

   def get_result(self, mem):
      """Load a cached result."""
      return None

   def add_result(self, mem, value):
      """Insert a result to the database."""
      pass

   def get_fpga_result(self, key):
      """Load FPGA timing data."""
      return None

   def add_fpga_result(self, key, frequency, bram_count):
      """Save FPGA timing data."""
      pass

   def set_value(self, key, value):
      """Set a value for the current state."""
      self.state[key] = value

   def get_value(self, key, default = None):
      """Get a value from the current state."""
      return self.state.get(key, default)

   def get_hash(self, value):
      """Get the hash for a value."""
      return hashlib.sha1(str(value)).hexdigest()

   def get_states(self):
      """Generator to return all persisted states."""
      raise StopIteration

   def remove(self, h):
      """Remove data for the specified hash."""
      pass

   def compact(self):
      """Perform database compaction."""
      pass

