from memsim import parser
from memsim.memory import base
from memsim.memory.dram import DRAM

class PCM(DRAM):

    def __init__(self, **kwargs):
        DRAM.__init__(self, **kwargs)

    def get_name(self, full):
        if not full:
            return DRAM.get_name(self, full)
        result = '(pcm '
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
        result += ')'
        return result


def _create_pcm(lexer, args):
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
    return PCM(frequency=frequency,
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
               ddr=ddr)
base.constructors['pcm'] = _create_pcm
