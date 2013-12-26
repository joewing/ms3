
from __future__ import print_function
import optparse
import sys

from memsim import distribution, lex, memory, model, process
from memsim.memory import stats


parser = optparse.OptionParser()
parser.add_option('-m', '--model', dest='model', default=None,
                  help='the model to use')
parser.add_option('-d', '--directory', dest='directory', default='.',
                  help='directory containing traces')


def main():
    options, args = parser.parse_args()
    if options.model is None:
        print('ERROR: no model specified')
        sys.exit(-1)
    with open(options.model) as f:
        m = model.parse_model(lex.Lexer(f))

    dists = []
    processes = []
    memories = []
    for i in range(len(m.benchmarks)):
        dist = distribution.Distribution(1)
        dists.append(dist)
        processes.append(process.Process(m.benchmarks[i]))
        memories.append(stats.Stats(dist, m.memory))
    pl = process.ProcessList(m.machine, processes, options.directory)
    ml = memory.MemoryList(memories)
    pl.run(ml, 0)

    for d in dists:
        min_addr = d.get_min_address()
        max_addr = d.get_max_address()
        size = d.get_size()
        print("[{}:{}): {}".format(min_addr, max_addr, size))


if __name__ == '__main__':
    main()
