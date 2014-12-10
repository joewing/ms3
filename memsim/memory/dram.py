from memsim import parser
from memsim.memory import base, main


class DRAM(main.MainMemory):
    """DRAM device model."""

    def __init__(self,
                 frequency,     # Frequency of the device
                 cas_cycles,    # CAS cycles
                 rcd_cycles,    # RAS-CAS cycles
                 rp_cycles,     # Precharge cycles
                 wb_cycles,     # Extra cycles for write-back
                 page_size,     # Size of a page in bytes
                 page_count,    # Number of pages per bank
                 width,         # Width of the channel in bytes
                 burst_size,    # Size of a burst in transfers
                 extra_cycles,  # Extra cycles per access
                 open_page,     # True for open-page, False for closed-page
                 ddr,           # True for DDR, False for SDR.
                 multiplier):   # Total cycle multiplier.
        main.MainMemory.__init__(self)
        self.frequency = frequency
        self.cas_cycles = cas_cycles
        self.rcd_cycles = rcd_cycles
        self.rp_cycles = rp_cycles
        self.wb_cycles = wb_cycles
        self.page_size = page_size
        self.page_count = page_count
        self.width = width
        self.burst_size = burst_size
        self.extra_cycles = extra_cycles
        self.open_page = open_page
        self.ddr = ddr
        self.multiplier = multiplier

    def get_name(self, full):
        if not full:
            return main.MainMemory.get_name(self, full)
        result = '(dram '
        result += '(frequency ' + str(self.frequency) + ')'
        result += '(cas_cycles ' + str(self.cas_cycles) + ')'
        result += '(rcd_cycles ' + str(self.rcd_cycles) + ')'
        result += '(rp_cycles ' + str(self.rp_cycles) + ')'
        result += '(wb_cycles ' + str(self.wb_cycles) + ')'
        result += '(page_size ' + str(self.page_size) + ')'
        result += '(page_count ' + str(self.page_count) + ')'
        result += '(width ' + str(self.width) + ')'
        result += '(burst_size ' + str(self.burst_size) + ')'
        if self.open_page:
            result += '(open_page true)'
        else:
            result += '(open_page false)'
        if self.ddr:
            result += '(ddr true)'
        else:
            result += '(ddr false)'
        if self.extra_cycles != 1.0:
            result += '(extra ' + str(self.extra_cycles) + ')'
        if self.multiplier != 1.0:
            result += '(multiplier ' + str(self.multiplier) + ')'
        result += ')'
        return result

    def get_word_size(self):
        return self.width * self.burst_size


def _create_dram(lexer, args):
    frequency = parser.get_argument(lexer, args, 'frequency', 666666667)
    cas_cycles = parser.get_argument(lexer, args, 'cas_cycles', 10)
    rcd_cycles = parser.get_argument(lexer, args, 'rcd_cycles', 10)
    rp_cycles = parser.get_argument(lexer, args, 'rp_cycles', 10)
    wb_cycles = parser.get_argument(lexer, args, 'wb_cycles', 0)
    page_size = parser.get_argument(lexer, args, 'page_size', 1024)
    page_count = parser.get_argument(lexer, args, 'page_count', 65536)
    width = parser.get_argument(lexer, args, 'width', 8)
    burst_size = parser.get_argument(lexer, args, 'burst_size', 4)
    extra_cycles = parser.get_argument(lexer, args, 'extra', 1.0)
    open_page = parser.get_argument(lexer, args, 'open_page', True)
    ddr = parser.get_argument(lexer, args, 'ddr', True)
    multiplier = parser.get_argument(lexer, args, 'multiplier', 1.0)
    return DRAM(frequency=frequency,
                cas_cycles=cas_cycles,
                rcd_cycles=rcd_cycles,
                rp_cycles=rp_cycles,
                wb_cycles=wb_cycles,
                page_size=page_size,
                page_count=page_count,
                width=width,
                burst_size=burst_size,
                extra_cycles=extra_cycles,
                open_page=open_page,
                ddr=ddr,
                multiplier=multiplier)
base.constructors['dram'] = _create_dram
