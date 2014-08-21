from __future__ import print_function
import os
import tempfile
import shutil
import re

from memsim import database, vhdl, cost
from memsim.memory import ram, memlist, subsystem


freq_regex = re.compile(r'Maximum Frequency: +([0-9\.]+)')
bram_regex = re.compile(r'Block RAM/FIFO: +([0-9]+)')
reg_regex = re.compile(r'Number of Slice Registers: +([0-9]+)')
lut_regex = re.compile(r'Number of Slice LUTs: +([0-9]+)')


class XilinxResult(object):

    def __init__(self,
                 frequency=1.0,
                 bram_count=1 << 31,
                 lut_count=1 << 31,
                 register_count=1 << 31):
        self.frequency = frequency
        self.bram_count = bram_count
        self.lut_count = lut_count
        self.register_count = register_count

    def get_pair(self):
        return (self.frequency, self.bram_count,
                self.lut_count, self.register_count)

    def __eq__(self, other):
        return self.get_pair() == other.get_pair()

    def __hash__(self):
        return hash(self.get_pair())


def run_xilinx(machine, mem, keep=False):
    """Get the results of running XST on the specified memory."""

    # Clone the memory so we can safely modify it.
    mem = mem.clone()

    # If we got a memory list, get timing for the complete
    # memory subsystem.  Otherwise, only report timing for the
    # specified component.
    if isinstance(mem, memlist.MemoryList):
        word_size = mem.get_main().get_word_size()
        main = ram.RAM(word_size=word_size, latency=0)
        ml = mem
    elif hasattr(mem, 'is_fifo'):
        word_size = mem.get_word_size()
        main = ram.RAM(word_size=word_size, latency=0)
        mem.set_next(main)
        ml = memlist.MemoryList(main)
        mem.index = 1
        ml.add_memory(mem)
    else:
        next_word_size = mem.get_next().get_word_size()
        main = ram.RAM(word_size=next_word_size, latency=0)
        mem.set_next(main)
        ml = memlist.MemoryList(main)
        ml.add_memory(subsystem.Subsystem(0, mem.get_word_size(), -1, mem))
    ml.set_main(main)
    name = machine.part + str(ml)

    # Determine if we've already processed this memory.
    db = database.get_instance()
    temp = db.get_fpga_result(name)
    if temp:
        return XilinxResult(temp[0], temp[1], temp[2], temp[3])

    print(name)

    # Create a directory for this run.
    old_dir = os.getcwd()
    dname = tempfile.mkdtemp(suffix='', prefix='ms')
    vhdl_file = dname + '/top.vhdl'
    project_file = dname + '/mem.prj'
    script_file = dname + '/mem.scr'
    ngc_file = dname + '/mem.ngc'
    result_file = dname + '/mem.srp'

    try:

        # Generate the HDL for the component.
        gen = vhdl.VHDLGenerator(machine)
        hdl = gen.generate(ml)
        with open(vhdl_file, 'w') as f:
            f.write(hdl)

        # Generate the XST project file.
        with open(project_file, 'w') as f:
            f.write('vhdl work ' + old_dir + '/hdl/adapter.vhdl\n')
            f.write('vhdl work ' + old_dir + '/hdl/arbiter.vhdl\n')
            f.write('vhdl work ' + old_dir + '/hdl/cache.vhdl\n')
            f.write('vhdl work ' + old_dir + '/hdl/combine.vhdl\n')
            f.write('vhdl work ' + old_dir + '/hdl/eor.vhdl\n')
            f.write('vhdl work ' + old_dir + '/hdl/fifo.vhdl\n')
            f.write('vhdl work ' + old_dir + '/hdl/offset.vhdl\n')
            f.write('vhdl work ' + old_dir + '/hdl/prefetch.vhdl\n')
            f.write('vhdl work ' + old_dir + '/hdl/shift.vhdl\n')
            f.write('vhdl work ' + old_dir + '/hdl/spm.vhdl\n')
            f.write('vhdl work ' + old_dir + '/hdl/split.vhdl\n')
            f.write('vhdl work ' + old_dir + '/hdl/ram.vhdl\n')
            f.write('vhdl work ' + vhdl_file + '\n')

        # Generate the XST script file.
        with open(script_file, 'w') as f:
            f.write("run -ifn " + project_file + " -ifmt mixed -top mem" +
                    " -ofn " + ngc_file + " -ofmt NGC -p " + machine.part +
                    " -ram_style block -opt_mode Speed -opt_level 2" +
                    " -register_balancing yes -keep_hierarchy no")

        # Run XST.
        os.chdir(dname)
        os.system("xst -ifn " + script_file + " >/dev/null 2>/dev/null")

        # Parse results.
        result = XilinxResult()
        with open(result_file, "r") as f:
            buf = f.read()
        m = freq_regex.search(buf)
        if m is None:
            raise Exception('Could not determine frequency')
        result.frequency = float(m.group(1)) * 1000000.0
        m = bram_regex.search(buf)
        if m is not None:
            result.bram_count = int(m.group(1))
        else:
            result.bram_count = 0
        m = reg_regex.search(buf)
        if m is None:
            raise Exception('Could not determine slice registers')
        result.register_count = int(m.group(1))
        m = lut_regex.search(buf)
        if m is None:
            raise Exception('Could not determine slice LUTs')
        result.lut_count = int(m.group(1))

        # Delete the project directory only if successful.
        os.chdir(old_dir)
        if keep:
            print("XST working directory:", dname)
        else:
            shutil.rmtree(dname)

        # Save and return the result.
        db.add_fpga_result(name, result.frequency, result.bram_count,
                           result.lut_count, result.register_count)
        return result

    except Exception as e:
        os.chdir(old_dir)
        print('ERROR: XST run failed:', e)
        print('ERROR: Memory:', mem)
        raise


def check_estimated_cost(machine, mem):
    """Check if the cost is reasonable enough to warrant simulation."""
    max_size = (machine.max_cost * 512 * 36) // 8
    return mem.get_bytes() <= max_size


def get_frequency(machine, mem):
    """Get the frequency of the specified memory."""
    return run_xilinx(machine, mem).frequency


def get_cost(machine, mem):
    """Get the cost of this memory component."""
    if not check_estimated_cost(machine, mem):
        return cost.Cost(cost=1 << 31)
    result = run_xilinx(machine, mem)
    if result.frequency >= machine.frequency:
        size = 0
        if hasattr(mem, 'is_fifo'):
            size = mem.total_size()
        return cost.Cost(cost=result.bram_count,
                         size=size,
                         luts=result.lut_count,
                         regs=result.register_count)
    else:
        return cost.Cost(cost=1 << 31)
