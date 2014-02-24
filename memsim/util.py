import re


def get_experiment_name(full_name):
    """Get the name of an experiment from it's full name."""
    base_name = re.sub(r'.*\/', '', full_name)
    return re.sub(r'-.*', '', base_name)


def log2(n):
    """Compute the log base 2 of n."""
    r = 0
    while n > 0:
        r += 1
        n >>= 1
    return r


def get_bus_shift(bus_size):
    return log2(bus_size) - 1


def align(word_size, addr):
    """Align addr to the next word boundary."""
    temp = addr & (word_size - 1)
    return addr + (word_size - temp) if temp != 0 else addr


def round_power2(n):
    """Round n up to the next highest power of 2."""
    return 1 << log2(n - 1)
