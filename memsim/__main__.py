
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


def main():
    options, args = parser.parse_args()
    if options.model is None:
        print('ERROR: no model specified')
        sys.exit(-1)
    m = model.parse_model_file(options.model)
    if not m:
        print('ERROR: could not read model')
        sys.exit(-1)
    if options.url is None:
        url = os.environ.get('COUCHDB_URL')
    else:
        url = options.url
    db = database.get_instance(m, url)
    db.set_value('model', str(m))
    print(m)

    distributions = []
    processes = []
    memories = []
    for i in range(len(m.benchmarks)):
        dist = distribution.Distribution(m.seed)
        dist.load(i)
        distributions.append(dist)
        processes.append(process.Process(dist, m.benchmarks[i]))
        memories.append(m.memory)

    pl = process.ProcessList(m.machine, processes, m.on, m.skip)
    ml = memory.MemoryList(memories, distributions)
    o = optimizer.Optimizer(m.machine, ml, m.seed,
                            use_prefetch=pl.has_delay())
    ml = o.load()
    limit = o.best_value * 4
    while o.evaluations < int(options.iterations):
        print('Iteration: {0} (steps: {1}, threshold: {2}, age: {3})'
              .format(o.evaluations + 1, o.steps + 1, o.threshold, o.age))
        print(ml)
        time = pl.run(ml, limit)
        print('Time:', time)
        print('Cost:', ml.get_cost())
        ml = o.optimize(time)
        print('Best Memory:', o.best_name)
        print('Best Value: ', o.best_value)
        print('Best Cost:   ', o.best_cost)
        db.save()
        gc.collect()

if __name__ == '__main__':
    main()
