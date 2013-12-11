
from __future__ import print_function
from StringIO import StringIO
import gc
import optparse
import os
import sys
import time

from memsim import (database, distribution, lex, memory, model,
                    optimizer, process)

parser = optparse.OptionParser()
parser.add_option('-u', '--url', dest='url', default=None,
                  help='database URL')
parser.add_option('-i', '--iterations', dest='iterations', default=1,
                  help='number of iterations for optimization')
parser.add_option('-m', '--model', dest='model', default=None,
                  help='model to use for optimization')
parser.add_option('-n', '--nosave', dest='nosave', default=False,
                  action='store_true',
                  help='do not save state to the database')
parser.add_option('-s', '--seed', dest='seed', default=None,
                  help='optimization random number seed')
parser.add_option('-d', '--directory', dest='directory', default=None,
                  help='directory containing trace data')


def show_best(db):
    best_name, best_value, best_cost = db.get_best()
    if best_name:
        print('Best Memory:', best_name)
        print('Best Value: ', best_value)
        print('Best Cost:  ', best_cost)
    return best_value


def main():
    options, args = parser.parse_args()
    if options.model is None:
        print('ERROR: no model specified')
        sys.exit(-1)
    m = model.parse_model_file(options.model)
    if not m:
        print('ERROR: could not read model')
        sys.exit(-1)
    directory = options.directory if options.directory else os.getcwd()
    db = database.get_instance(options.url)
    db.load(m)
    print(m)

    seed = int(options.seed) if options.seed else int(time.time())
    distributions = []
    processes = []
    memories = []
    for i in xrange(len(m.benchmarks)):
        dist = distribution.Distribution(seed)
        dist.load(db, i)
        distributions.append(dist)
        processes.append(process.Process(dist, m.benchmarks[i]))
        memories.append(m.memory)

    # Load the result from the first run.
    best_name, t, _ = db.get_best()
    if best_name:
        lexer = lex.Lexer(StringIO(best_name))
        ml = memory.parse_memory_list(lexer, distributions)
        first = not db.has_data()
    else:
        ml = memory.MemoryList(memories, distributions)
        first = True

    pl = process.ProcessList(first, m.machine, processes, directory,
                             m.on, m.skip)
    if first:
        t = pl.run(ml, 0)
    o = optimizer.Optimizer(m.machine, ml, seed, use_prefetch=pl.has_delay())
    o.load(db)
    if first:
        o.save(db)
    ml = o.optimize(db, t)
    while True:
        best_value = show_best(db)
        result_count = db.get_result_count()
        if result_count >= int(options.iterations):
            break
        print('Evaluation:', result_count + 1)
        print(ml)
        t = pl.run(ml, 10 * best_value)
        print('Time: {} (cost: {})'.format(t, ml.get_cost()))
        ml = o.optimize(db, t)
        gc.collect()


if __name__ == '__main__':
    main()
