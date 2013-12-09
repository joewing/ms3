
from __future__ import print_function
import gc
import optparse
import os
import sys

from memsim import database, distribution, memory, model, optimizer, process

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
parser.add_option('-s', '--seed', dest='seed', default=7,
                  help='optimization random number seed')
parser.add_option('-d', '--directory', dest='directory', default=None,
                  help='directory containing trace data')


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
    db.set_value('model', str(m))
    print(m)

    seed = int(options.seed)
    distributions = []
    processes = []
    memories = []
    for i in xrange(len(m.benchmarks)):
        dist = distribution.Distribution(seed)
        dist.load(i)
        distributions.append(dist)
        processes.append(process.Process(dist, m.benchmarks[i]))
        memories.append(m.memory)

    pl = process.ProcessList(m.machine, processes, directory, m.on, m.skip)
    ml = memory.MemoryList(memories, distributions)
    o = optimizer.Optimizer(m.machine, ml, seed, use_prefetch=pl.has_delay())
    ml = o.load()
    while o.evaluations < int(options.iterations):
        print('Iteration: {0} (steps: {1}, threshold: {2}, age: {3})'
              .format(o.evaluations + 1, o.steps + 1, o.threshold, o.age))
        print(ml)
        time = pl.run(ml, o.best_value * 2)
        print('Time:', time)
        print('Cost:', ml.get_cost())
        ml = o.optimize(time)
        print('Best Memory:', o.best_name)
        print('Best Value: ', o.best_value)
        print('Best Cost:  ', o.best_cost)
        if not options.nosave:
            db.save()
        gc.collect()

if __name__ == '__main__':
    main()
