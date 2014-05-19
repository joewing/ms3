from __future__ import print_function
import os
import tempfile
import shutil
import re

from memsim import database
from memsim import vhdl
from memsim.memory import ram, memlist, subsystem


freq_regex = re.compile("Maximum Frequency: +([0-9\.]+)")
bram_regex = re.compile("Block RAM/FIFO: +([0-9]+)")


class XilinxResult(object):

    def __init__(self, frequency=1.0, bram_count=1 << 31):
        self.frequency = frequency
        self.bram_count = bram_count

    def get_pair(self):
        return (self.frequency, self.bram_count)

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
        return XilinxResult(temp[0], temp[1])

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
            raise Exception("Could not determine frequency")
        result.frequency = float(m.group(1)) * 1000000.0
        m = bram_regex.search(buf)
        if m is not None:
            result.bram_count = max(1, int(m.group(1)))

        # Delete the project directory only if successful.
        os.chdir(old_dir)
        if keep:
            print("XST working directory:", dname)
        else:
            shutil.rmtree(dname)

        # Save and return the result.
        db.add_fpga_result(name, result.frequency, result.bram_count)
        return result

    except Exception as e:
        print('ERROR: XST run failed:', e)
        print('ERROR: Memory:', mem)
        raise


def get_frequency(machine, mem):
    """Get the frequency of the specified memory."""
    return run_xilinx(machine, mem).frequency


def get_bram_count(machine, mem):
    """Get the number of BRAMs for this memory component."""
    result = run_xilinx(machine, mem)
    if result.frequency >= machine.frequency:
        return result.bram_count
    else:
        return 1 << 31
