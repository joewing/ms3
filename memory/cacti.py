
import math
import os
import re
import subprocess
import sys
import tempfile

import cache
import spm

class CACTIResult:
   area = 0
   time = 0

   def __eq__(self, other):
      return (self.area, self.time) == (other.area, other.time)

   def __hash__(self):
      return hash((self.area, self.time))

class CACTIParams:
   size = 0
   block_size = 0
   bus_bits = 0
   associativity = 1
   is_cache = False

   def get_pair(self):
      p = (self.size, self.block_size, self.bus_bits,
           self.associativity, self.is_cache)
      return p

   def __eq__(self, other):
      return self.get_pair() == other.get_pair()

   def __hash__(self):
      return hash(self.get_pair())

cacti_results = dict()
area_regex = re.compile("Data array: Area \(mm2\): ([0-9.]+)")
time_regex = re.compile("Access time \(ns\): ([0-9.]+)")

def generate_file(fd, params):
   """Generate the input file for CACTI."""
   fd.write("-size (bytes) " + str(params.size) + "\n")
   fd.write("-block size (bytes) " + str(params.block_size) + "\n")
   fd.write("-associativity " + str(params.associativity) + "\n")
   fd.write("-read-write port 1\n")
   fd.write("-exclusive read port 0\n")
   fd.write("-exclusive write port 0\n")
   fd.write("-single ended read ports 0\n")
   fd.write("-UCA bank count 1\n")
   fd.write("-technology (u) 0.032\n")
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

def run_cacti(params):
   """Get the result of running CACTI with the specified parameters."""

   # Check if we already tried a memory with these parameters.
   if params in cacti_results:
      return cacti_results[params]

   # Make sure the cacti program exists and is executable.
   cacti_exe = './cacti'
   if not os.path.isfile(cacti_exe):
      print("ERROR: " + cacti_exe + " not found")
      sys.exit(-1)
   if not os.access(cacti_exe, os.X_OK):
      print("ERROR: " + cacti_exe + " not executable")
      sys.exit(-1)

   # Generate a file containing the parameters for CACTI.
   fd, file_name = tempfile.mkstemp(suffix = '.cacti',
                                    prefix = 'ms',
                                    dir = None,
                                    text = True)
   with os.fdopen(fd, 'w') as f:
      generate_file(f, params)

   # Run CACTI.
   try:
      buf = subprocess.check_output(['./cacti', '-infile', file_name])
   except subprocess.CalledProcessError:
      buf = ''
   finally:
      os.remove(file_name)

   # Extract the area and time from the CACTI results.
   result = CACTIResult()
   m = area_regex.search(buf)
   if m == None:
      result.area = 1 << 31
   else:
      result.area = int(math.ceil(float(m.group(1)) * 1000.0 * 1000.0))
   m = time_regex.search(buf)
   if m == None:
      result.time = 1 << 31
   else:
      result.time = int(math.ceil(float(m.group(1))))

   cacti_results[params] = result
   return result

def get_cache_params(machine, mem):
   """Get the CACTI parameters for a cache."""
   params = CACTIParams()
   params.size = machine.word_size * mem.line_count * mem.line_size
   params.block_size = machine.word_size * mem.line_size
   params.bus_bits = machine.addr_bits + machine.word_size * 8
   params.associativity = mem.associativity
   params.is_cache = True
   return params

def get_spm_params(machine, mem):
   """Get the CACTI parameters for an SPM."""
   params = CACTIParams()
   params.size = mem.size
   params.block_size = machine.word_size
   params.bus_bits = 8 * machine.word_size
   params.is_cache = False
   return params

def get_params(machine, mem):
   """Get the CACTI parameters for a memory."""
   if isinstance(mem, cache.Cache):
      return get_cache_params(machine, mem)
   elif isinstance(mem, spm.SPM):
      return get_spm_params(machine, mem)
   else:
      assert(False)

def get_results(machine, mem):
   """Get the results of a CACTI run for the specified memory."""
   return run_cacti(get_params(machine, mem))

def get_area(machine, mem):
   """Get the area for the specified memory (shallow)."""
   return get_results(machine, mem).area

def get_time(machine, mem):
   """Get the time for the specified memory (shallow)."""
   return get_results(machine, mem).time

