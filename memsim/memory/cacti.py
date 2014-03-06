from __future__ import print_function
import math
import os
import re
import subprocess
import sys
import tempfile

from memsim import database


class CACTIResult(object):

    def __init__(self, access_time=0, cycle_time=0, area=0):
        self.access_time = access_time
        self.cycle_time = cycle_time
        self.area = area

    def get_pair(self):
        return (self.access_time, self.cycle_time, self.area)

    def __eq__(self, other):
        return self.get_pair() == other.get_pair()

    def __hash__(self):
        return hash(self.get_pair())


class CACTIParams(object):

    size = 0
    block_size = 0
    bus_bits = 0
    associativity = 1
    is_cache = False
    technology = 0.045

    def __str__(self):
        result = "("
        if self.is_cache:
            result += "cache "
            result += "(associativity " + str(self.associativity) + ")"
        else:
            result += "sram "
        result += "(size " + str(self.size) + ")"
        result += "(block_size " + str(self.block_size) + ")"
        result += "(bus_bits " + str(self.bus_bits) + ")"
        result += "(technology " + str(self.technology) + ")"
        result += ")"
        return result

    def get_pair(self):
        p = (self.size, self.block_size, self.bus_bits,
             self.associativity, self.is_cache, self.technology)
        return p

    def __eq__(self, other):
        return self.get_pair() == other.get_pair()

    def __hash__(self):
        return hash(self.get_pair())


cacti_results = dict()
area_regex = re.compile("Data array: Area \(mm2\): ([0-9.]+)")
access_regex = re.compile("Access time \(ns\): +([0-9.]+)")
cycle_regex = re.compile("Cycle time \(ns\): +([0-9.]+)")


def _generate_file(fd, params):
    """Generate the input file for CACTI."""
    fd.write("-size (bytes) " + str(params.size) + "\n")
    fd.write("-block size (bytes) " + str(params.block_size) + "\n")
    fd.write("-associativity " + str(params.associativity) + "\n")
    fd.write("-read-write port 1\n")
    fd.write("-exclusive read port 0\n")
    fd.write("-exclusive write port 0\n")
    fd.write("-single ended read ports 0\n")
    fd.write("-UCA bank count 1\n")
    fd.write("-technology (u) " + str(params.technology) + "\n")
    fd.write("-Data array cell type - \"itrs-hp\"\n")
    fd.write("-Data array peripheral type - \"itrs-hp\"\n")
    fd.write("-Tag array cell type - \"itrs-hp\"\n")
    fd.write("-Tag array peripheral type - \"itrs-hp\"\n")
    fd.write("-output/input bus width " + str(params.bus_bits) + "\n")
    fd.write("-operating temperature (K) 350\n")
    if params.is_cache:
        fd.write("-cache type \"cache\"\n")
    else:
        fd.write("-cache type \"ram\"\n")
    fd.write("-tag size (b) \"default\"\n")
    fd.write("-access mode (normal, sequential, fast) - \"normal\"\n")
    fd.write("-Cache model (NUCA, UCA) - \"UCA\"\n")
    fd.write("-design objective (weight delay, dynamic power, " +
             "leakage power, cycle time, area) 0:0:0:0:100\n")
    fd.write("-deviate (delay, dynamic power, leakage power, " +
             "cycle time, area) 60:100000:100000:100000:1000000\n")
    fd.write("-Print level (DETAILED, CONCISE) - \"DETAILED\"\n")
    fd.write("-internal prefetch width 8\n")


def _find_cacti():
    """Find the cacti executable."""
    for path in os.environ['PATH'].split(os.pathsep):
        path = path.strip('"')
        cacti_exe = os.path.join(path, 'cacti')
        if os.path.isfile(cacti_exe) and os.access(cacti_exe, os.X_OK):
            return cacti_exe
    print('ERROR: cacti not found')
    sys.exit(-1)
    return None


def _run_cacti(params):
    """Get the result of running CACTI with the specified parameters."""

    # Check if we already tried a memory with these parameters.
    db = database.get_instance()
    temp = db.get_cacti_result(params)
    if temp:
        return CACTIResult(access_time=temp[0],
                           cycle_time=temp[1],
                           area=temp[2])

    # Find cacti.
    cacti_exe = _find_cacti()

    # Generate a file containing the parameters for CACTI.
    fd, file_name = tempfile.mkstemp(suffix='.cacti',
                                     prefix='ms',
                                     dir=None,
                                     text=True)
    with os.fdopen(fd, 'w') as f:
        _generate_file(f, params)

    # Run CACTI.
    try:
        buf = subprocess.check_output([cacti_exe, '-infile', file_name])
    except subprocess.CalledProcessError:
        buf = ''
    finally:
        os.remove(file_name)

    # Extract the area, access time, and cycle time from the CACTI results.
    result = CACTIResult()
    m = area_regex.search(buf)
    if m:
        result.area = int(math.ceil(float(m.group(1)) * 1000.0 * 1000.0))
    else:
        result.area = 1 << 31
    m = access_regex.search(buf)
    result.access_time = float(m.group(1)) if m else (1 << 31)
    m = cycle_regex.search(buf)
    result.cycle_time = float(m.group(1)) if m else result.access_time

    db.add_cacti_result(params,
                        result.access_time,
                        result.cycle_time,
                        result.area)
    return result


def _get_cache_params(machine, mem):
    """Get the CACTI parameters for a cache."""
    next_word_size = mem.get_next().get_word_size()
    next_word_width = next_word_size * 8
    params = CACTIParams()
    params.technology = machine.technology
    params.size = mem.line_count * mem.line_size
    params.block_size = mem.line_size
    params.bus_bits = min(next_word_width, mem.line_size * 8)
    params.associativity = mem.associativity
    params.is_cache = True
    return params


def _get_spm_params(machine, mem):
    """Get the CACTI parameters for an SPM."""
    params = CACTIParams()
    params.technology = machine.technology
    params.size = mem.size
    params.block_size = mem.get_word_size()
    params.bus_bits = 8 * mem.get_word_size()
    params.is_cache = False
    return params


def _get_params(machine, mem):
    """Get the CACTI parameters for a memory."""
    if mem.__class__.__name__ == 'Cache':
        return _get_cache_params(machine, mem)
    elif mem.__class__.__name__ == 'SPM':
        return _get_spm_params(machine, mem)
    else:
        assert(False)


def _get_results(machine, mem):
    """Get the results of a CACTI run for the specified memory."""
    return _run_cacti(_get_params(machine, mem))


def get_area(machine, mem):
    """Get the area for the specified memory (shallow)."""
    return _get_results(machine, mem).area


def get_cycles(machine, t):
    """Convert from time in nanoseconds to cycles."""
    freq_ghz = float(machine.frequency) / 1000000000.0
    return int(math.ceil(t * freq_ghz))


def get_cycle_time(machine, mem):
    """Get the cycle time in cycles for the specified memory (shallow)."""
    return get_cycles(machine, _get_results(machine, mem).cycle_time)


def get_access_time(machine, mem):
    """Get the access time in cycles for the specified memory (shallow)."""
    return get_cycles(machine, _get_results(machine, mem).access_time)
