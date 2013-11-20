
from __future__ import print_function
import optparse
import os
import sys
import StringIO

from memsim import database, lex, memory, model
from memsim.memory import xilinx


parser = optparse.OptionParser()
parser.add_option('-u', '--url', dest='url', default=None,
                  help='database URL')
parser.add_option('-k', '--keep', dest='keep', default=False,
                        action='store_true',
                        help='keep intermediate files')
parser.add_option('-m', '--memory', dest='memory', default='model',
                  help='evaluate "best", "baseline", or "model"')
parser.add_option('-b', '--baseline', dest='baseline', default=None,
                  help='file containing the baseline memory')


def get_frequency(experiment, mem, baseline, keep):
    m = model.parse_model_file(experiment)
    db = database.get_instance()
    db.load(m)
    if mem == 'model':
        pass
    elif mem == 'baseline':
        with open(baseline, 'r') as f:
            m.memory = memory.parse_memory(lex.Lexer(f))
    elif mem == 'best':
        best_name = db.get_value('best_name', str(m.memory))
        best_file = StringIO.StringIO(best_name)
        m.memory = memory.parse_memory(lex.Lexer(best_file))
    else:
        print('ERROR: invalid memory selected:', mem)
    m.machine.frequency = 1 << 31
    result = xilinx.run_xilinx(m.machine, m.memory, keep, True)
    print(experiment + ',' + str(result.frequency) + ',' +
          str(result.bram_count))


def main():
    options, args = parser.parse_args()
    if not args:
        print('ERROR: no model(s) specified')
        sys.exit(-1)
    url = options.url if options.url else os.environ.get('COUCHDB_URL')
    if not database.get_instance(url):
        print('ERROR: could not connect to the database')
        sys.exit(-1)
    for experiment in args:
        get_frequency(experiment, options.memory,
                      options.baseline, options.keep)


if __name__ == '__main__':
    main()
