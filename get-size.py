
from __future__ import print_function
import optparse
import sys

from memsim import distribution, lex, memory, model
from memsim.memory import stats
from memsim.sim import evaluate


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

    # Divide up the address space.
    total_size = 1 << m.machine.addr_bits
    fifo_size = sum(map(lambda f: f.total_size(), m.fifos))
    proc_size = total_size - fifo_size
    size = proc_size // len(m.benchmarks)

    dists = []
    ml = memory.MemoryList(m.memory)
    for b in m.benchmarks:
        dist = distribution.Distribution(1)
        dists.append(dist)
        ml.add_memory(stats.Stats(dist, m.memory))
    evaluate(m, ml, options.directory)

    for d in dists:
        min_addr = d.get_min_address()
        max_addr = d.get_max_address()
        size = d.get_size()
        access_count = d.get_access_count()
        print("{} accesses; [{}:{}): {}"
              .format(access_count, min_addr, max_addr, size))


if __name__ == '__main__':
    main()
