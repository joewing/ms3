
from __future__ import print_function
from StringIO import StringIO
import atexit
import multiprocessing
import optparse
import gc
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
    sim,
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
    thread_count = 0
    pool = None


main_context = MainContext()


def show_status(key, name, best_value, best_cost, evaluation, status):
    data = main_context.data
    data[key] = (name, best_value, best_cost, evaluation, status)
    thread_count = main_context.thread_count
    request_count = main_context.server.request_count
    send_count = main_context.server.db.send_count
    print()
    print('Threads: {}    Database requests: {} / {}'
          .format(thread_count, request_count, send_count))
    print('  {:<20}{:<12}{:<12}{:<12}{}'
          .format('name', 'value', 'cost', 'evaluation', 'status'))
    for ident in data:
        marker = '> ' if key == ident else '  '
        name, value, cost, ev, stat = data[ident]
        print('{:<2}{:<20}{:<12}{:<12}{:<12}{}'
              .format(marker, name, value, cost, ev, stat))
    print()


def get_initial_memory(db, m, dists, directory):
    """Get the initial subsystem and its total access time."""

    # First attempt to load the best subsystem from the database.
    best_name, best_value, _ = db.get_best(m)
    if best_name:

        # Load statistics from the database.
        state = db.load(m)
        use_prefetch = state['use_prefetch']
        for i, d in enumerate(dists):
            d.load(state, i)

        # Create the initial memory subsystem.
        lexer = lex.Lexer(StringIO(best_name))
        ml = memory.parse_memory_list(lexer, m.memory)
        return ml, best_value, use_prefetch

    # No previous memory subsystem is stored, so we need run the
    # empty memory subsystem to collect statistics.

    # Divide up the address space.
    total_size = 1 << m.machine.addr_bits
    fifo_size = sum(map(lambda f: f.total_size(), m.fifos))
    proc_size = total_size - fifo_size
    size = proc_size // len(m.benchmarks)

    # Create a memory subsystem to collect statistics.
    ml = memory.MemoryList(m.memory)
    pl = sim.ProcessList(m.machine, directory)
    dist_index = 0
    for f in m.fifos:
        pl.add_fifo(f)
        ml.add_memory(stats.Stats(dists[dist_index], m.memory))
        dist_index += 1
    for i, b in enumerate(m.benchmarks):
        pl.add_benchmark(b, size)
        ml.add_memory(stats.Stats(dists[dist_index], m.memory))
        dist_index += 1

    # Collect statistics and get the execution time.
    best_value = pl.run(ml)

    # Save statistics to the database.
    state = dict()
    use_prefetch = pl.has_delay()
    state['use_prefetch'] = use_prefetch
    for i, d in enumerate(dists):
        d.save(state, i)
    db.save(m, state)

    # Return the empty memory subsystem and execution time.
    ml = memory.MemoryList(m.memory)
    for i in xrange(len(dists)):
        ml.add_memory(m.memory)
    return ml, best_value, use_prefetch


def optimize(db, mod, iterations, seed, directory):

    # Divide up the address space.
    total_size = 1 << mod.machine.addr_bits
    fifo_size = sum(map(lambda f: f.total_size(), mod.fifos))
    proc_size = total_size - fifo_size
    size = proc_size // len(mod.benchmarks)

    # Create the random number distributions to use for modifying
    # the memory subsystems and create the benchmark processes.
    dists = []
    pl = sim.ProcessList(mod.machine, directory)
    for f in mod.fifos:
        dists.append(distribution.Distribution(seed))
        pl.add_fifo(f)
    for i, b in enumerate(mod.benchmarks):
        dists.append(distribution.Distribution(seed))
        pl.add_benchmark(b, size)

    # Load the first memory to use.
    # This will gather statistics if necessary.
    ml, t, use_prefetch = get_initial_memory(db, mod, dists, directory)

    # Perform the optimization.
    o = MemoryOptimizer(mod, ml, seed, dists, use_prefetch)
    while True:

        # Show the best and get its value.
        result_count = db.get_result_count(mod)
        _, best_value, best_cost = db.get_best(mod)
        db.update_status(best_value, best_cost, result_count, str(o))

        # Exit if we've performed enough evaluations.
        if result_count >= iterations:
            break

        # Get the next subsystem to evaluate.
        ml = o.optimize(db, t)
        if ml is None:
            # Another process is working on this value.
            break

        # Evaluate the memory subsystem.
        t = pl.run(ml.simplified())

        gc.collect()


def run_experiment(db, mod, iterations, seed, directory):

    # Wrap the execution in a try block so we can start a new thread
    # if something bad happens (most likely missing cacti or xst).
    try:
        database.set_instance(db)
        optimize(db, mod, iterations, seed, directory)
    except KeyboardInterrupt:
        return -1
    except:
        traceback.print_exc()
    return db.ident


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
        print('ERROR: could not read model:', experiment)
        return False

    # Only start the thread if there is work to do.
    if server.db.get_result_count(m) >= iterations:
        return False

    # Create a shared database instance.
    name = os.path.basename(experiment)
    db = server.add_client(name)

    print('Starting {}'.format(name))

    # Start the thread.
    args = {
        'db': db,
        'iterations': iterations,
        'mod': m,
        'directory': directory,
        'seed': seed,
    }
    pool = main_context.pool
    main_context.thread_count += 1
    pool.apply_async(run_experiment, kwds=args, callback=experiment_done)
    return True


def experiment_done(ident):
    """Signal that an experiment has completed; start the next."""

    # Check if we should exit.
    if ident < 0:
        main_context.thread_count -= 1
        return

    # Log a message.
    data = main_context.data[ident]
    print('Finished {}'.format(data[0]))

    # Remove this client.
    main_context.server.remove_client(ident)
    del main_context.data[ident]

    # Start the next experiment.
    for _ in xrange(len(main_context.experiments)):
        if start_experiment(main_context):
            break

    # Update the number of active threads.
    # We do this after starting the next experiment to ensure
    # that the main thread doesn't exit too soon.
    main_context.thread_count -= 1


@atexit.register
def handle_term():
    global main_context
    print('Exiting')
    if main_context.pool is not None:
        main_context.pool.terminate()


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
    manager = multiprocessing.Manager()
    main_context.server = DatabaseServer(manager, db, show_status)

    # Create the thread pool.
    max_threads = int(options.threads)
    main_context.pool = multiprocessing.Pool(max_threads)

    # Start the initial set of experiments.
    # Additional experiments will be run as running experiments complete.
    try:
        max_tries = len(main_context.experiments)
        started = 0
        tries = 0
        while started < max_threads and tries < max_tries:
            tries += 1
            if start_experiment(main_context):
                started += 1
                tries = 0
            main_context.server.run()

        # Process database traffic and update status.
        while main_context.thread_count > 0:
            while main_context.server.run():
                pass
            time.sleep(0.125)
        main_context.pool.terminate()
    except KeyboardInterrupt:
        main_context.pool.terminate()
    main_context.pool = None
    print('Done')
    sys.exit(0)


if __name__ == '__main__':
    main()
