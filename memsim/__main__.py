
from __future__ import print_function
from StringIO import StringIO
import multiprocessing
import optparse
import os
import sys
import time
import traceback

from memsim import (
    database,
    distribution,
    lex,
    memory,
    model,
    process,
)
from memsim.memory import stats
from memsim.memopt import MemoryOptimizer
from memsim.database.server import DatabaseServer


usage = 'usage: %prog [options] experiment1 ... experimentn'
parser = optparse.OptionParser()
parser.add_option('-u', '--url', dest='url', default=None,
                  help='database URL')
parser.add_option('-i', '--iterations', dest='iterations', default=1,
                  help='number of iterations for optimization')
parser.add_option('-s', '--seed', dest='seed', default=None,
                  help='optimization random number seed')
parser.add_option('-d', '--directory', dest='directory', default='.',
                  help='directory containing trace data')
parser.add_option('-t', '--threads', dest='threads', default=1,
                  help='number of threads')


class MainContext(object):
    experiments = []
    directory = '.'
    seed = 0
    iterations = 0
    server = None
    data = dict()


main_context = MainContext()


def show_status(key, name, best_value, best_cost, evaluation, status):
    better = ' '
    data = main_context.data
    if key in data:
        name, last_value, last_cost, _, _ = data[key]
        if last_value != best_value or last_cost != best_cost:
            better = '*'
    data[key] = (name, best_value, best_cost, evaluation, status)
    print('\x1b[2J\x1b[H')
    thread_count = main_context.server.get_client_count()
    request_count = main_context.server.request_count
    print('Threads: {}    Database requests: {}'
          .format(thread_count, request_count))
    print()
    print('  {:<20}{:<12}{:<12}{:<12}{}'
          .format('name', 'value', 'cost', 'evaluation', 'status'))
    for ident in data:
        marker = (better + '>') if key == ident else '  '
        name, value, cost, ev, stat = data[ident]
        print('{:<2}{:<20}{:<12}{:<12}{:<12}{}'
              .format(marker, name, value, cost, ev, stat))


def get_initial_memory(db, m, dists, directory):
    """Get the initial subsystem and its total access time."""

    # First attempt to load the best subsystem from the database.
    best_name, best_value, _ = db.get_best(m)
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


def optimize(db, mod, iterations, seed, directory):

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

    # Perform the optimization.
    o = MemoryOptimizer(mod, ml, seed, dists,
                        use_prefetch=pl.has_delay())
    o.load(db)
    while True:

        # Show the best and get its value.
        result_count = db.get_result_count(mod)
        _, best_value, best_cost = db.get_best(mod)
        db.update_status(best_value, best_cost, result_count, str(o))

        # Exit if we've performed enough evaluations.
        if result_count >= iterations:
            break

        # Evaluate this memory subsystem.
        ml = o.optimize(db, t).simplified()
        t = pl.run(ml, 10 * best_value)


def run_experiment(db, mod, iterations, seed, directory):

    # Wrap the execution in a try block so we can start a new thread
    # if something bad happens (most likely missing cacti or xst).
    try:
        database.set_instance(db)
        optimize(db, mod, iterations, seed, directory)
    except:
        traceback.print_exc()

    # Signal that this thread is exiting.
    db.signal_exit()


def start_experiment(context):

    seed = context.seed
    server = context.server
    iterations = context.iterations
    directory = context.directory
    experiments = context.experiments

    # Determine the experiment to run and update the seed.
    context.seed = (seed + 1) % (2 << 31)
    experiment_count = len(experiments)
    experiment = experiments[seed % experiment_count]

    # Load the model and its current state.
    m = model.parse_model_file(experiment)
    if not m:
        print('ERROR: could not read model')
        sys.exit(-1)

    # Only start the thread if there is work to do.
    if server.db.get_result_count(m) >= iterations:
        return None

    # Create a shared database instance.
    name = os.path.basename(experiment)
    db = server.add_client(name)

    # Start the thread.
    kwargs = {
        'db': db,
        'iterations': iterations,
        'mod': m,
        'directory': directory,
        'seed': seed,
    }
    return multiprocessing.Process(target=run_experiment, kwargs=kwargs)


def signal_exit(key):
    """Signal that a process has exited; start the next."""
    main_context.server.remove_client(key)
    for i in xrange(len(main_context.experiments)):
        proc = start_experiment(main_context)
        if proc is not None:
            proc.start()
            break


def main():

    global main_context

    # Parse arguments.
    options, args = parser.parse_args()
    if args is None:
        print('ERROR: no experiment specified')
        sys.exit(-1)
    main_context.experiments = args
    main_context.directory = options.directory
    main_context.seed = int(options.seed) if options.seed else int(time.time())
    main_context.iterations = int(options.iterations)
    db = database.get_instance(options.url)

    # Create the database server.
    main_context.server = DatabaseServer(db, show_status, signal_exit)

    # Run the experiments.
    max_threads = int(options.threads)
    thread_count = 0
    max_tries = int(options.threads) * int(len(main_context.experiments))
    tries = 0
    while thread_count < max_threads and tries < max_tries:
        proc = start_experiment(main_context)
        if proc is not None:
            proc.start()
            thread_count += 1
        tries += 1
        while main_context.server.run():
            pass

    # Process database traffic and update status.
    while True:
        if not main_context.server.run():
            time.sleep(0.25)


if __name__ == '__main__':
    main()
