
class CacheItem(object):
    def __init__(self, key, value):
        self.key = key
        self.value = value
        self.next = None
        self.prev = None


class ResultCache(object):
    """Fixed-size result cache (works like a dictionary)."""

    def __init__(self, max_size):
        """Create a result cache for caching up to max_size results."""
        self.max_size = max_size
        self.mapping = {}
        self.oldest = None
        self.newest = None

    def _insert(self, item):
        """Insert an item to the head of the linked list."""
        item.next = self.newest
        item.prev = None
        if self.newest is None:
            self.oldest = item
        else:
            self.newest.prev = item
        self.newest = item

    def _remove(self, item):
        """Remove an item from the linked list."""
        if item.next is None:
            self.oldest = item.prev
        else:
            item.next.prev = item.prev
        if item.prev is None:
            self.newest = item.next
        else:
            item.prev.next = item.next

    def __setitem__(self, key, value):
        """Set a value."""

        # Remove the oldest mapping if there are too many.
        if len(self.mapping) >= self.max_size:
            del self.mapping[self.oldest.key]
            self._remove(self.oldest)

        # Remove duplicates.
        if key in self.mapping:
            dup = self.mapping[key]
            del self.mapping[key]
            self._remove(dup)

        # Create an item and add it to our list.
        item = CacheItem(key, value)
        self._insert(item)

        # Add the new item to the dictionary.
        self.mapping[key] = item

    def __getitem__(self, key):
        """Get a value.  Returns None if not found."""

        # Look up the item.
        item = self.mapping.get(key)
        if item is None:
            return None

        # Move the item to the head of the list.
        self._remove(item)
        self._insert(item)

        # Return the value.
        return item.value

    def __contains__(self, key):
        """Determine if a value is in the cache."""
        return key in self.mapping
