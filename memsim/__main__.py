
from __future__ import print_function
from StringIO import StringIO
import threading
import optparse
import os
import sys
import time

from memsim import (
    database,
    distribution,
    lex,
    memory,
    model,
    process,
    util,
)
from memsim.memory import stats
from memsim.memopt import MemoryOptimizer


usage = 'usage: %prog [options] experiment1 ... experimentn'
parser = optparse.OptionParser()
parser.add_option('-u', '--url', dest='url', default=None,
                  help='database URL')
parser.add_option('-i', '--iterations', dest='iterations', default=1,
                  help='number of iterations for optimization')
parser.add_option('-s', '--seed', dest='seed', default=None,
                  help='optimization random number seed')
parser.add_option('-d', '--directory', dest='directory', default=None,
                  help='directory containing trace data')
parser.add_option('-t', '--threads', dest='threads', default=1,
                  help='number of threads')


class ThreadData(object):

    def __init__(self):
        self.data = dict()
        self.cond = threading.Condition()
        self.cond.acquire()
        self.stats_lock = threading.Lock()

    def release(self):
        self.cond.release()

    def stop_thread(self):
        self.cond.acquire()
        self.thread = threading.current_thread()
        self.cond.notify()
        self.cond.release()

    def wait(self):
        self.cond.wait()
        self.thread.join()

    def show_status(self, best_value, best_cost, evaluation):
        self.stats_lock.acquire()
        ident = threading.current_thread().ident
        self.data[ident] = (best_value, best_cost, evaluation)
        print('{:<16}{:<12}{:<12}{:<12}'
              .format('name', 'value', 'cost', 'evaluation'))
        for thrd in threading.enumerate():
            ident = thrd.ident
            if ident in self.data:
                name = thrd.name
                value, cost, ev = self.data[ident]
                print('{:<16}{:<12}{:<12}{:<12}'
                      .format(name, value, cost, ev))
        print()
        self.stats_lock.release()


def get_initial_memory(db, m, dists, directory):
    """Get the initial subsystem and its total access time."""

    # First attempt to load the best subsystem from the database.
    best_name, best_value, _ = db.get_best()
    if best_name:
        lexer = lex.Lexer(StringIO(best_name))
        ml = memory.parse_memory_list(lexer)
        return ml, best_value

    # No previous memory subsystem is stored, so we need run the
    # empty memory subsystem and collect statistics.

    # Create a memory subsystem to collect statistics.
    processes = []
    memories = []
    for i in xrange(len(m.benchmarks)):
        memories.append(stats.Stats(dists[i], m.memory))
        processes.append(process.Process(m.benchmarks[i]))
    ml = memory.MemoryList(memories)
    pl = process.ProcessList(m.machine, processes, directory)

    # Collect statistics and get the execution time.
    best_value = pl.run(ml, 0)

    # Return the empty memory subsystem and execution time.
    ml = memory.MemoryList(map(lambda _: m.memory, m.benchmarks))
    return ml, best_value


def run_experiment(url, mod, iterations, seed, directory, tdata):

    # Connect to the database (each thread needs its own database object).
    db = database.get_instance(url)
    db.load(mod)

    # Create the random number distributions to use for modifying
    # the memory subsystems and create the benchmark processes.
    dists = []
    procs = []
    for i in xrange(len(mod.benchmarks)):
        dists.append(distribution.Distribution(seed))
        procs.append(process.Process(mod.benchmarks[i]))
    pl = process.ProcessList(mod.machine, procs, directory)

    # Load the first memory to use.
    # This will gather statistics if necessary.
    ml, t = get_initial_memory(db, mod, dists, directory)

    # Create the optimizer object and load state.
    o = MemoryOptimizer(mod.machine, ml, seed, dists,
                        use_prefetch=pl.has_delay())
    o.load(db)

    # Perform the optimization.
    while True:

        # Show the best and get its value.
        result_count = db.get_result_count()
        _, best_value, best_cost = db.get_best()
        tdata.show_status(best_value, best_cost, result_count)

        # Exit if we've performed enough evaluations.
        if result_count >= iterations:
            break

        # Evaluate this memory subsystem.
        ml = o.optimize(db, t).simplified()
        t = pl.run(ml, 10 * best_value)

    tdata.stop_thread()


def start_experiment(url, directory, seed, iterations, experiment, tdata):

    # Load the model and its current state.
    m = model.parse_model_file(experiment)
    if not m:
        print('ERROR: could not read model')
        sys.exit(-1)
    db = database.get_instance(url)
    db.load(m)

    # Only start the thread if there is work to do.
    if db.get_result_count() >= iterations:
        return None

    # Start the thread.
    kwargs = {
        'url': url,
        'iterations': iterations,
        'mod': m,
        'directory': directory,
        'seed': seed,
        'tdata': tdata,
    }
    tname = util.get_experiment_name(experiment)
    return threading.Thread(target=run_experiment, name=tname, kwargs=kwargs)


def main():

    # Parse arguments.
    options, args = parser.parse_args()
    if args is None:
        print('ERROR: no experiment specified')
        sys.exit(-1)
    directory = options.directory if options.directory else os.getcwd()
    seed = int(options.seed) if options.seed else int(time.time())
    iterations = int(options.iterations)
    url = options.url

    # Run the experiments.
    tdata = ThreadData()
    max_threads = int(options.threads)
    threads = []
    while True:
        started_thread = False
        for experiment in args:
            thrd = start_experiment(url, directory, seed, iterations,
                                    experiment, tdata)
            if thrd is not None:
                thrd.start()
                threads.append(thrd)
                started_thread = True
            if len(threads) >= max_threads:
                tdata.wait()
                threads = filter(lambda t: t.is_alive(), threads)
        if not started_thread:
            break
    tdata.release()


if __name__ == '__main__':
    main()
