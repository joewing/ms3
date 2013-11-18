
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
parser.add_option('-c', '--compare', dest='compare', default=False,
                  action='store_true',
                  help='generate comparison matrix')


def simulate(url, experiment, mem, baseline):
    m = model.parse_model_file(experiment)
    db = database.get_instance(m, url)
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
    m.skip = 0
    m.on = 1000000
    print(m)
    time = evaluate(m)
    print(experiment + ',' + str(time))


def generate_array(url, experiments, mem, baseline):
    for experiment in experiments:
        simulate(url, experiment, mem, baseline)


def generate_matrix(url, experiments, mem, baseline):
    if mem != 'best':
        print('WARN: using', mem, 'memory')
    for mem_model in experiments:
        m = model.parse_model_file(mem_model)
        db = database.get_instance(m, url)
        best_name = db.get_value('best_name', str(m.memory))
        best_file = StringIO.StringIO(best_name)
        model_memory = memory.parse_memory(lex.Lexer(best_file))
        for experiment in experiments:
            m = model.parse_model_file(experiment)
            db = database.get_instance(m, url)
            if mem == 'model':
                temp = model.parse_model_file(mem_model)
                m.memory = temp.memory
            elif mem == 'baseline':
                with open(baseline, 'r') as f:
                    m.memory = memory.parse_memory(lex.Lexer(f))
            elif mem == 'best':
                m.memory = model_memory
            m.skip = 0
            m.on = 1000000
            time = evaluate(m)
            print(experiment + ',' + mem_model + ',' + str(time))


def main():
    global db
    options, args = parser.parse_args()
    if len(args) == 0:
        print('ERROR: no models specified')
        sys.exit(-1)
    url = options.url if options.url else os.environ.get('COUCHDB_URL')
    if options.compare:
        generate_matrix(url, args, options.memory, options.baseline)
    else:
        generate_array(url, args, options.memory, options.baseline)


if __name__ == '__main__':
    main()
