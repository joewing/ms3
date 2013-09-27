
import random

class Distribution(random.Random):

   # Minimum range size in bytes.
   min_size = 8

   # List of ranges, which are pairs of the form (start, length).
   ranges = []

   # Stack of address range limits of the form (False, lower limit, upper limit)
   # and address transforms, which are functions from address to
   # transformed address.  Transform entries are pairs: (True, function).
   limits = []

   def __init__(self, seed):
      super(random.Random, self).__init__(seed)
      self.start_seed = seed

   def reset(self):
      """Reset the random number generator using the original seed."""
      self.seed(self.start_seed)

   def _check_overlap(self, a, b):
      """Determine if two ranges overlap."""
      a1, a2 = a[0], a[0] + a[1] + self.min_size
      b1, b2 = b[0], b[0] + b[1] + self.min_size
      # a1 b1 a2 # b1 a1 b2
      return (a1 <= b1 and b1 <= a2) or (b1 <= a1 and a1 <= b2)

   def _extend_range(self, a, b):
      """Create a range that contains both a and b."""
      start = min(a[0], b[0])
      a_end = a[0] + a[1]
      b_end = b[0] + b[1]
      size = max(a_end, b_end) - start
      return (start, size)

   def insert_range(self, addr, size):
      """Insert a new range into the set of address ranges."""

      # Check if this address already exists.
      for i in range(len(self.ranges)):
         r = self.ranges[i]
         start, end = r
         if addr >= start and addr < end + self.min_size:
            if addr + size > end:
               # This address extends the range.
               self.ranges[i] = (start, addr - start + size)
            return

      # If we get here, we have a new range.
      new = (addr, size)

      # Coalesce ranges.
      for i in reversed(range(len(self.ranges))):
         other = self.ranges[i]
         if self._check_overlap(new, other):
            new = self._extend_range(new, other)
            del self.ranges[i]

      # Insert the new range.
      self.ranges.append(new)

   def push_limit(self, lower, upper):
      """Push an address limit on to the stack.
         This is done when evaluating a part of a split or scratchpad
         to limit the addresses that are selected.
      """
      self.limits.append((False, lower, upper))

   def pop_limit(self):
      """Pop an address limit off of the stack."""
      self.limits.pop()

   def push_transform(self, transform):
      """Push an address transform on to the stack."""
      self.limits.append((True, transform))

   def pop_transform(self):
      """Pop an address transform off of the stack."""
      self.limits.pop()

   def _get_weighted_value(self, start, size, alignment):
      """Get a random address."""
      addr = start
      nsize = size
      while nsize > alignment:
         rand = self.randint(0, 7)
         if rand == 0:
            return addr
         elif rand == 1:
            return addr + nsize - alignment
         elif rand < 5:
            nsize = (nsize + 1) / 2
         else:
            addr += nsize / 2
            nsize = (nsize + 1) / 2
      return addr

   def random_address(self, alignment):
      """Get a random address with the specified alignment."""

      # We make multiple attempts to pick a valid address.
      # If we fail, we just return a random address since it's
      # possible that no valid addresses exist.
      for i in range(100):

         # Select a random range.
         rindex = self.randint(0, len(self.ranges) - 1)
         r = self.ranges[rindex]

         # Select an address in the range.
         addr = self._get_weighted_value(r[0], r[1], alignment)

         # Transform the address and check validity.
         valid = True
         for l in self.limits:
            if l[0]:
               transform = l[1]
               addr = transform(addr)
            elif addr < l[1] or addr > l[2]:
               valid = False

         # Enforce alignment.
         # This could push the address outside of the range.
         addr -= addr % alignment

         # Exit the loop if we have a valid address.
         if valid: break;

      # Here we either have a valid random address or, at least,
      # some address.
      return addr

   def randbool(self):
      """Generate a random boolean value."""
      return self.randint(0, 1) == 1

