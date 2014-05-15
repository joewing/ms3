
class AccessType(object):
    """Enumeration of possible memory access types."""
    READ = 0        # Read from memory
    WRITE = 1       # Write to memory
    MODIFY = 2      # Modify
    IDLE = 3        # Idle
    CONSUME = 4     # Consume a value from an input port
    PRODUCE = 5     # Produce a value on an output port
    PEEK = 6        # Peek at a value on an input port
    INPUT = 7       # Input a value from one of two ports
    OUTPUT = 8      # Output a value to one of two ports
    END = 9         # Produce a value indicating the end of a stream
