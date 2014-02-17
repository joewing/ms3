from memsim.sim.dist import BaseDistribution


class MemoryDistribution(BaseDistribution):

    # Minimum range size in bytes.
    min_size = 1024

    def __init__(self, rand):
        BaseDistribution.__init__(self, rand)

        # List of ranges, which are pairs of the form (start, length).
        self.ranges = []

    def load(self, state, index):
        temp = state.get('memdist' + str(index))
        if temp is not None:
            self.ranges = state.get('memdist' + str(index))

    def save(self, state, index):
        state['memdist' + str(index)] = self.ranges

    def is_empty(self):
        return len(self.ranges) == 0

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
        for i in xrange(len(self.ranges)):
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
        for i in reversed(xrange(len(self.ranges))):
            other = self.ranges[i]
            if self._check_overlap(new, other):
                new = self._extend_range(new, other)
                del self.ranges[i]

        # Insert the new range.
        self.ranges.append(new)

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
                nsize = (nsize + 1) // 2
            else:
                addr += nsize // 2
                nsize = (nsize + 1) // 2
        return addr

    def get_min_address(self):
        """Get the minimum address encountered."""
        return self.ranges[0][0]

    def get_max_address(self):
        """Get the maximum address encountered."""
        r = self.ranges[-1]
        return r[0] + r[1]

    def get_size(self):
        """Get the total size of accesses."""
        return sum(map(lambda r: r[1], self.ranges))

    def random_address(self, alignment):

        # Handle the case when there are no addresses.
        if len(self.ranges) == 0:
            return 0

        # We make multiple attempts to pick a valid address.
        # If we fail, we just return a random address since it's
        # possible that no valid addresses exist.
        for i in xrange(100):

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
            if valid:
                break

        # Here we either have a valid random address or, at least,
        # some address.
        return addr
