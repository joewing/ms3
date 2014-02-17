from memsim.sim.proclist import ProcessList


def evaluate(mod, ml, directory):
    """Evaluate the specified memory."""
    total_size = 1 << mod.machine.addr_bits
    fifo_size = sum(mod.fifos)
    proc_size = total_size - fifo_size
    size = proc_size // len(mod.benchmarks)
    pl = ProcessList(mod.machine, directory)
    for b in mod.benchmarks:
        pl.add_benchmark(b, size)
    return pl.run(ml), ml.get_cost()
