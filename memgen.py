from __future__ import print_function
import optparse
import sys

from memsim import model, vhdl


parser = optparse.OptionParser()
parser.add_option('-d', '--directory', dest='directory', default='',
                  help='directory containing trace data')


def main():
    options, args = parser.parse_args()
    if len(args) < 1:
        print('ERROR: no model file specified')
        sys.exit(-1)
    elif len(args) > 1:
        print('ERROR: too many options')
        sys.exit(-1)
    m = model.parse_model_file(args[0])
    for b in m.benchmarks:
        mem = m.memory.get_subsystem(b.index)
        if mem.depth < 0:
            word_size = mem.get_word_size()
            total_size = b.get_size(options.directory)
            mem.depth = total_size // word_size
            assert(mem.depth >= 0)
    gen = vhdl.VHDLGenerator(m.machine)
    result = gen.generate(m.memory)
    print(result)


if __name__ == '__main__':
    main()
