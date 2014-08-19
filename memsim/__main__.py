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
import signal
import threading

from memsim import (
    database,
    lex,
    memory,
    model,
    sim,
)
from memsim.memory import stats
from memsim.memopt import MemoryOptimizer
from memsim.optimizer import PendingException
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
parser.add_option('-v', '--verbose', dest='verbose', default=False,
                  action='store_true', help='be verbose')


class MainContext(object):
    experiments = []
    directory = '.'
    seed = 0
    iterations = 0
    server = None
    data = dict()
    thread_count = 0
    pool = None
    verbose = False
    stop = False
    lock = threading.Lock()


main_context = MainContext()


def show_status(key, name, best_value, best_cost, evaluation, status):
    """Show current status (runs from the main process)."""
    if main_context.verbose:
        return
    with main_context.lock:
        data = main_context.data
        data[key] = (name, best_value, best_cost, evaluation, status)
        thread_count = main_context.thread_count
        request_count = main_context.server.request_count
        send_count = main_context.server.db.send_count
        print()
        print('Threads: {}    Database requests: {} / {}'
              .format(thread_count, request_count, send_count))
        print('  {:<20}{:<12}{:<20}{:<12}{}'
              .format('name', 'value', 'cost', 'evaluation', 'status'))
        for ident in data:
            marker = '> ' if key == ident else '  '
            name, value, cost, ev, stat = data[ident]
            print('{:<2}{:<20}{:<12}{:<20}{:<12}{}'
                  .format(marker, name, value, cost, ev, stat))
        print()


def get_total_value(value):
    """Aggregate value for multiple subsystems."""
    return sum(value.values())


def get_subsystem_values(db, m, ml, directory):
    pl = sim.ProcessList(m.machine, directory)
    for b in m.benchmarks:
        pl.add_benchmark(b)
    result = dict()
    for b in m.benchmarks:
        subsystem = b.index
        mem = ml.get_subsystem(subsystem)
        value = db.get_result(m, mem, subsystem)
        if value is None:
            value = pl.run(ml, subsystem)
            db.add_result(m, mem, subsystem, value, mem.get_cost())
        result[subsystem] = value
    return result


def get_initial_memory(db, m, dist, directory):
    """Get the initial subsystem and its total access time.

    This runs from a process in the process pool.
    """

    # Load the model from the database.
    # If not found, we need to collect statistics first.
    state = db.load(m)
    if len(state) == 0:
        if main_context.verbose:
            print('Collecting statistics')
        for b in m.benchmarks:
            mem = m.memory.get_subsystem(b.index)
            sd = dist.get_subsystem_distribution(mem)
            b.collect_stats(directory, sd)
            dist.save(state)
            db.save(m, state)

    # Attempt to load the best subsystem from the database.
    # If not found, we need to evaluate it.
    best_name, _, _ = db.get_best(m)
    if best_name:

        # Load statistics from the database.
        state = db.load(m)
        dist.load(state, m)

        # Create the initial memory subsystem.
        lexer = lex.Lexer(StringIO(best_name))
        ml = memory.parse_memory_list(lexer)

        # Load the values for each subsystem.
        return ml, get_subsystem_values(db, m, ml, directory)

    # Get the current value.
    if main_context.verbose:
        print('Initial Memory: {}'.format(m.memory))
    ml = m.memory.clone()
    best_value = get_subsystem_values(db, m, ml, directory)
    total = get_total_value(best_value)
    ml.reset(m.machine)
    db.insert_best(m, ml, total, ml.get_cost())
    if main_context.verbose:
        print('Value: {}'.format(total))
        print('Cost: {}'.format(ml.get_cost()))
    return get_initial_memory(db, m, dist, directory)


def optimize(db, mod, iterations, seed, directory):

    # Create the random number distributions to use for modifying
    # the memory subsystems and create the benchmark processes.
    dist = sim.DistributionList(seed)
    pl = sim.ProcessList(mod.machine, directory)
    for b in mod.benchmarks:
        pl.add_benchmark(b)

    # Load the first memory to use.
    # This will gather statistics if necessary.
    last_ml, values = get_initial_memory(db, mod, dist, directory)
    last_ml.reset(mod.machine)
    best_cost = last_ml.get_cost()
    best_value = get_total_value(values)
    result_count = db.get_result_count(mod)

    # Perform the optimization.
    o = MemoryOptimizer(mod, best_value, seed, dist, directory)
    db.update_status(best_value, best_cost, result_count, str(o))
    while True:

        # Get the next subsystem to try.
        try:
            ml, subsystem = o.get_next(last_ml)
        except PendingException:
            return True

        # Evaluate the current memory subsystem.
        simplified = ml.simplified()
        new_values = get_subsystem_values(db, mod, simplified, directory)
        total = get_total_value(new_values)
        cost = simplified.get_cost()

        # Update the best.
        updated = False
        lower_cost = cost.fits(best_cost)
        if total < best_value or (total == best_value and lower_cost):
            best_value = total
            best_cost = cost
            db.update_best(mod, simplified, total, cost)
            updated = True

        # Request the best and result count only after evaluating
        # a new state or updating the best.
        if updated:
            result_count = db.get_result_count(mod)
            db.update_status(best_value, best_cost, result_count, str(o))
            if main_context.verbose:
                print('Best Value:  {}'.format(best_value))
                print('Best Cost:   {}'.format(best_cost))
            if result_count >= iterations:
                return False
            if main_context.verbose:
                print('Iteration {} / {}'.format(result_count + 1, iterations))

        # Update the optimizer.
        try:
            if o.update(total):
                last_ml = ml
        except PendingException:
            # Another process is working on this value.
            return True


def run_experiment(db, mod, iterations, seed, directory):

    # Wrap the execution in a try block so we can start a new thread
    # if something bad happens (most likely missing cacti or xst).
    try:
        database.set_instance(db)
        while optimize(db, mod, iterations, seed, directory):
            pass
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


def experiment_init():
    signal.signal(signal.SIGINT, signal.SIG_IGN)


def experiment_done(ident):
    """Signal that an experiment has completed."""

    if ident < 0:
        main_context.stop = True
        return

    with main_context.lock:
        print('Finished', ident)
        main_context.thread_count -= 1
        main_context.server.remove_client(ident)
        del main_context.data[ident]


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
    main_context.verbose = options.verbose
    db = database.get_instance(options.url)

    # Create the database server.
    manager = multiprocessing.Manager()
    main_context.server = DatabaseServer(manager, db, show_status)

    # Create the thread pool.
    max_threads = int(options.threads)
    main_context.verbose = main_context.verbose or max_threads == 1
    main_context.pool = multiprocessing.Pool(max_threads, experiment_init)

    # Run the experiments, starting new ones as necessary.
    try:
        active = True
        while not main_context.stop:
            while main_context.server.run():
                pass
            if active and main_context.thread_count < max_threads:
                active = False
                for _ in main_context.experiments:
                    if start_experiment(main_context):
                        active = True
                        break
            elif main_context.thread_count == 0:
                main_context.stop = True
                break
            gc.collect()
            time.sleep(0.125)
        main_context.pool.terminate()
    except KeyboardInterrupt:
        main_context.pool.terminate()
    main_context.pool = None
    print('Done')
    sys.exit(0)


if __name__ == '__main__':
    main()
