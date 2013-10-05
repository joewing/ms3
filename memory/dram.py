
import base
import parser

class DRAMBank:
   page     = -1
   dirty    = False
   pending  = 0

class DRAM(base.Memory):

   def __init__(self,
                multiplier,
                cas_cycles,
                rcd_cycles,
                rp_cycles,
                wb_cycles,
                page_size,
                page_count,
                width,
                burst_size,
                open_page_mode):
      self.multiplier = multiplier
      self.cas_cycles = cas_cycles
      self.rcd_cycles = rcd_cycles
      self.rp_cycles = rp_cycles
      self.wb_cycles = wb_cycles
      self.page_size = page_size
      self.page_count = page_count
      self.width = width
      self.burst_size = burst_size
      self.open_page_mode = open_page_mode
      self.pending = 0
      self.banks = list()

   def __str__(self):
      result  = '(dram '
      result += '(multiplier ' + str(self.multiplier) + ')'
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

   def process(self, write, addr, size):
      assert(size > 0)
      bsize = self.burst_size * self.width
      last = addr + size - 1
      while addr <= last:
         temp = addr - (addr % bsize) + bsize
         self._do_process(write, addr & self.machine.addr_mask, temp >= last)
         addr = temp
      return 0

   def _do_process(self, write, addr, is_last):

      # Get the bank.
      bank_size = self.page_size * self.page_count
      bank_index = addr // bank_size
      bank = self.banks[bank_index]

      # Make sure this bank is ready for another request.
      if self.machine.time < bank.pending:
         self.machine.time = bank.pending

      extra = 0
      cycles = 0
      page_index = addr // self.page_size
      if not self.open_page_mode:
         # Closed page mode.
         cycles += self.cas_cycles * self.multiplier
         cycles += self.rcd_cycles * self.multiplier
         cycles += self.burst_size * self.multiplier
         extra += self.rp_cycles * self.multiplier
         if write:
            extra += self.wb_cycles * self.multiplier
      elif bank.page == page_index:
         # Page hit.
         cycles += self.cas_cycles * self.multiplier
         cycles += self.burst_size * self.multiplier
         bank.dirty = bank.dirty or write
      else:
         # Page miss.
         cycles += self.rp_cycles * self.multiplier
         cycles += self.rcd_cycles * self.multiplier
         cycles += self.cas_cycles * self.multiplier
         cycles += self.burst_size * self.multiplier
         if bank.dirty:
            cycles += self.wb_cycles * self.multiplier
         bank.dirty = write
      self.machine.time += cycles
      bank.pending = self.machine.time + extra
      bank.page = page_index

def _create_dram(args):
   multiplier = parser.get_argument(args, 'multiplier', 1)
   cas_cycles = parser.get_argument(args, 'cas_cycles', 10)
   rcd_cycles = parser.get_argument(args, 'rcd_cycles', 10)
   rp_cycles = parser.get_argument(args, 'rp_cycles', 10)
   wb_cycles = parser.get_argument(args, 'wb_cycles', 0)
   page_size = parser.get_argument(args, 'page_size', 1024)
   page_count = parser.get_argument(args, 'page_count', 16384)
   width = parser.get_argument(args, 'width', 16)
   burst_size = parser.get_argument(args, 'burst_size', 4)
   open_page_mode = parser.get_argument(args, 'open_page_mode', True)
   return DRAM(multiplier = multiplier,
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

