
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


def round_power2(n):
    """Round n up to the next highest power of 2."""
    return 1 << log2(n - 1)
