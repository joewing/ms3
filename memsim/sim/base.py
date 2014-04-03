from memsim.sim.proclist import ProcessList


def evaluate(mod, ml, directory, fast):
    """Evaluate the specified memory."""
    pl = ProcessList(mod.machine, directory)
    for b in mod.benchmarks:
        pl.add_benchmark(b)
    return pl.run(ml, fast), ml.get_cost()
