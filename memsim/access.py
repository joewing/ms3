
class AccessType(object):
    """Enumeration of possible memory access types."""
    READ = 0        # Read from memory
    WRITE = 1       # Write to memory
    IDLE = 2        # Idle
    CONSUME = 3     # Consume a value from an input port
    PRODUCE = 4     # Produce a value on an output port
    PEEK = 5        # Peek at a value on an input port
    END = 6         # Produce a value indicating the end of a stream
