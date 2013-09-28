
from Memory import Memory

class DRAM(Memory):

   def __init__(self,
                cas_cycles,
                rcd_cycles,
                rp_cycles,
                wb_cycles,
                page_size,
                page_count,
                width,
                burst_size,
                open_page_mode):
      self.cas_cycles = cas_cycles
      self.rcd_cycles = rcd_cycles
      self.rp_cycles = rp_cycles
      self.wb_cycles = wb_cycles
      self.page_size = page_size
      self.page_count = page_count
      self.width = width
      self.burst_size = burst_size
      self.open_page_mode = open_page_mode

   def __str__(self):
      result  = '(dram '
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

