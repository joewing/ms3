
from __future__ import print_function
import optparse
import os
import sys
import StringIO

from memsim import database, lex, memory, model
from memsim.process import evaluate


parser = optparse.OptionParser()
parser.add_option('-u', '--url', dest='url', default=None,
                  help='database URL')
parser.add_option('-m', '--memory', dest='memory', default='model',
                  help='evaulate "best", "baseline", or "model"')
parser.add_option('-b', '--baseline', dest='baseline', default=None,
                  help='file containing the baseline memory')


def simulate(url, experiment, mem, baseline):
    m = model.parse_model_file(experiment)
    db = database.get_instance(m, url)
    if mem == 'model':
        pass
    elif mem == 'baseline':
        m.memory = memory.parse(lex.Lexer(baseline))
    elif mem == 'best':
        best_name = db.get_value('best_name', str(m.memory))
        best_file = StringIO.StringIO(best_name)
        m.memory = memory.parse_memory(lex.Lexer(best_file))
    else:
        print('invalid memory selected:', mem)
    m.skip = 0
    m.on = 1000000
    time = evaluate(m)
    print(experiment + ',' + str(time))


def main():
    global db
    options, args = parser.parse_args()
    if len(args) == 0:
        print('ERROR: no models specified')
        sys.exit(-1)
    url = options.url if options.url else os.environ.get('COUCHDB_URL')
    for experiment in args:
        simulate(url, experiment, options.memory, options.baseline)


if __name__ == '__main__':
    main()
