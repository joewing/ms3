
from __future__ import print_function
import optparse
import sys

from memsim import distribution
from memsim import lex
from memsim import memory
from memsim import model
from memsim import process


parser = optparse.OptionParser()
parser.add_option('-m', '--model', dest='model', default=None,
                        help='the model to use')


def main():
    options, args = parser.parse_args()
    if options.model is None:
        print('ERROR: no model specified')
        sys.exit(-1)
    with open(options.model) as f:
        m = model.parse_model(lex.Lexer(f))

    distributions = []
    processes = []
    memories = []
    for i in range(len(m.benchmarks)):
        dist = distribution.Distribution(m.seed)
        dist.load(i)
        distributions.append(dist)
        processes.append(process.Process(dist, m.benchmarks[i]))
        memories.append(m.memory)
    pl = process.ProcessList(m.machine, processes, 1000000, 0)
    ml = memory.MemoryList(memories, distributions)
    pl.run(ml, 0)

    for d in distributions:
        min_addr = d.get_min_address()
        max_addr = d.get_max_address()
        size = d.get_size()
        print("[{}:{}): {}".format(min_addr, max_addr, size))


if __name__ == '__main__':
    main()
