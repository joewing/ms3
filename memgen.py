from __future__ import print_function
from StringIO import StringIO
import optparse
import sys

from memsim import database, lex, model, vhdl
from memsim.memory import memlist


parser = optparse.OptionParser()
parser.add_option('-d', '--directory', dest='directory', default='',
                  help='directory containing trace data')
parser.add_option('-u', '--url', dest='url', default=None,
                  help='database URL')
parser.add_option('-b', '--best', dest='best', default=False,
                  action='store_true',
                  help='output the best subsystem for the model')


def main():
    options, args = parser.parse_args()
    if len(args) < 1:
        print('ERROR: no model file specified')
        sys.exit(-1)
    elif len(args) > 1:
        print('ERROR: too many model files specified')
        sys.exit(-1)
    m = model.parse_model_file(args[0])
    if options.best:
        db = database.get_instance(options.url)
        best, value, cost = db.get_best(m)
        if best is None:
            print('ERROR: Model not found')
            sys.exit(-1)
        m.memory = memlist.parse_memory_list(lex.Lexer(StringIO(best)))
        print('-- Cost: {}, value: {}'.format(cost, value))
    name = '-- ' + str(m).replace(') (', ')\n --    (')
    name = name.replace(')(benchmarks ', ')\n-- (benchmarks ')
    print(name)
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
