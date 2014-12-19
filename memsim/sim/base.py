from memsim.sim.proclist import ProcessList


def evaluate(mod, ml, directory, fast):
    """Evaluate the specified memory."""
    pl = ProcessList(mod.machine, directory)
    for b in mod.benchmarks:
        pl.add_benchmark(b)
    return pl.run(ml, -1), ml.get_cost(mod.machine)
