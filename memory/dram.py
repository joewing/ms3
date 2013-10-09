
import math
import base
import parser

class DRAMBank:
   page     = -1        # Currently open page.
   dirty    = False     # Set if the open page is dirty.
   time     = 0         # Time of the next allowed access.

class DRAM(base.Memory):
   """DRAM device model.
      We can model DDR by doubling the width parameter.
   """

   def __init__(self,
                frequency,       # Frequency of the device
                cas_cycles,      # CAS cycles
                rcd_cycles,      # RAS-CAS cycles
                rp_cycles,       # Precharge cycles
                wb_cycles,       # Extra cycles for write-back
                page_size,       # Size of a page in bytes
                page_count,      # Number of pages per bank
                width,           # Width of the channel in bytes
                burst_size,      # Size of a burst in bytes
                open_page_mode): # True for open-page, False for closed-page
      self.frequency = frequency
      self.cas_cycles = cas_cycles
      self.rcd_cycles = rcd_cycles
      self.rp_cycles = rp_cycles
      self.wb_cycles = wb_cycles
      self.page_size = page_size
      self.page_count = page_count
      self.width = width
      self.burst_size = burst_size
      self.open_page_mode = open_page_mode
      self.banks = list()

   def __str__(self):
      result  = '(dram '
      result += '(frequency ' + str(self.frequency) + ')'
      result += '(cas_cycles ' + str(self.cas_cycles) + ')'
      result += '(rcd_cycles ' + str(self.rcd_cycles) + ')'
      result += '(rp_cycles ' + str(self.rp_cycles) + ')'
      result += '(wb_cycles ' + str(self.wb_cycles) + ')'
      result += '(page_size ' + str(self.page_size) + ')'
      result += '(page_count ' + str(self.page_count) + ')'
      result += '(width ' + str(self.width) + ')'
      result += '(burst_size ' + str(self.burst_size) + ')'
      if self.open_page_mode:
         result += '(open_page_mode true)'
      else:
         result += '(open_page_mode false)'
      result += ')'
      return result

   def reset(self, machine):
      base.Memory.reset(self, machine)
      bank_size = self.page_size * self.page_count
      bank_count = (machine.addr_mask + bank_size) // bank_size
      self.banks = list()
      for i in range(bank_count):
         self.banks.append(DRAMBank())
      self.multiplier = float(machine.frequency) / float(self.frequency)

   def process(self, start, write, addr, size):
      assert(size > 0)

      # Convert machine time to DRAM time.
      delta = float(start) / self.multiplier

      # Process each part of the request.
      bsize = self.burst_size * self.width
      last = addr + size - 1
      while addr <= last:
         temp = addr - (addr % bsize) + bsize
         addr &= self.machine.addr_mask
         delta = self._do_process(delta, write, addr, temp >= last)
         addr = temp

      # Convert DRAM time back to machine time.
      return int(math.ceil(delta * self.multiplier))

   def _do_process(self, delta, write, addr, is_last):

      # Get the bank.
      bank_size = self.page_size * self.page_count
      bank_index = addr // bank_size
      bank = self.banks[bank_index]

      # Convert machine time to DRAM time.
      # Note that delta is already in DRAM time.
      mtime = self.machine.time / self.multiplier

      # Make sure this bank is ready for another request.
      if mtime + delta < bank.time:
         delta = bank.time - mtime

      extra = 0
      page_index = addr // self.page_size
      if not self.open_page_mode:
         # Closed page mode.
         delta += self.cas_cycles
         delta += self.rcd_cycles
         delta += self.burst_size
         extra += self.rp_cycles
         if write:
            extra += self.wb_cycles
      elif bank.page == page_index:
         # Page hit.
         delta += self.cas_cycles
         delta += self.burst_size
         bank.dirty = bank.dirty or write
      else:
         # Page miss.
         delta += self.rp_cycles
         delta += self.rcd_cycles
         delta += self.cas_cycles
         delta += self.burst_size
         if bank.dirty:
            delta += self.wb_cycles
         bank.dirty = write
      bank.time = mtime + delta + extra
      bank.page = page_index
      return delta

def _create_dram(args):
   frequency = parser.get_argument(args, 'frequency', 666666667)
   cas_cycles = parser.get_argument(args, 'cas_cycles', 10)
   rcd_cycles = parser.get_argument(args, 'rcd_cycles', 10)
   rp_cycles = parser.get_argument(args, 'rp_cycles', 10)
   wb_cycles = parser.get_argument(args, 'wb_cycles', 0)
   page_size = parser.get_argument(args, 'page_size', 1024)
   page_count = parser.get_argument(args, 'page_count', 65536)
   width = parser.get_argument(args, 'width', 16)
   burst_size = parser.get_argument(args, 'burst_size', 4)
   open_page_mode = parser.get_argument(args, 'open_page_mode', True)
   return DRAM(frequency = frequency,
               cas_cycles = cas_cycles,
               rcd_cycles = rcd_cycles,
               rp_cycles = rp_cycles,
               wb_cycles = wb_cycles,
               page_size = page_size,
               page_count = page_count,
               width = width,
               burst_size = burst_size,
               open_page_mode = open_page_mode)
base.constructors['dram'] = _create_dram

