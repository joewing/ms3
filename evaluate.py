
from __future__ import print_function
import optparse
import sys

from memsim import distribution, memory, model, process


parser = optparse.OptionParser()
parser.add_option('-m', '--model', dest='model', default=None,
                  help='model to evaluate')


def main():
    options, args = parser.parse_args()
    if options.model is None:
        print('ERROR: no model specified')
        sys.exit(-1)
    m = model.parse_model_file(options.model)
    if not m:
        print('ERROR: could not read model')
        sys.exit(-1)

    distributions = []
    processes = []
    memories = []
    for i in range(len(m.benchmarks)):
        dist = distribution.Distribution(m.seed)
        distributions.append(dist)
        processes.append(process.Process(dist, m.benchmarks[i]))
        memories.append(m.memory)

    pl = process.ProcessList(m.machine, processes, 100000, 0)
    ml = memory.MemoryList(memories, distributions)
    time = pl.run(ml, 0)
    print('Time:', time)


if __name__ == '__main__':
    main()
