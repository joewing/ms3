
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
parser.add_option('-d', '--directory', dest='directory', default=None,
                  help='directory containing trace data')


def simulate(experiment, mem, baseline, directory):
    m = model.parse_model_file(experiment)
    db = database.get_instance()
    db.load(m)
    if mem == 'model':
        subsystem = m.memory
    elif mem == 'baseline':
        with open(baseline, 'r') as f:
            subsystem = memory.parse_memory(lex.Lexer(f))
    elif mem == 'best':
        best_name = db.get_value('best_name', str(m.memory))
        best_file = StringIO.StringIO(best_name)
        subsystem = memory.parse_memory(lex.Lexer(best_file))
    else:
        print('ERROR: invalid memory selected:', mem)
        sys.exit(-1)
    fixup_model(m)
    time = None
    if db.load(m):
        time = db.get_result(subsystem)
    if time is None:
        m.memory = subsystem
        time = evaluate(m, directory)
        db.add_result(subsystem, time)
    print(experiment + ',' + str(time))


def fixup_model(m):
    m.skip = 0
    m.on = 1000000


def generate_array(experiments, mem, baseline, directory):
    for experiment in experiments:
        simulate(experiment, mem, baseline, directory)


def generate_matrix(experiments, mem, baseline, directory):
    if mem != 'best':
        print('WARN: using', mem, 'memory')
    for mem_model in experiments:
        m = model.parse_model_file(mem_model)
        db = database.get_instance()
        if not db.load(m):
            print('WARN: no best for', m)
        best_name = db.get_value('best_name', str(m.memory))
        best_file = StringIO.StringIO(best_name)
        model_memory = memory.parse_memory(lex.Lexer(best_file))
        for experiment in experiments:
            m = model.parse_model_file(experiment)
            if mem == 'model':
                temp = model.parse_model_file(mem_model)
                subsystem = temp.memory
            elif mem == 'baseline':
                with open(baseline, 'r') as f:
                    subsystem = memory.parse_memory(lex.Lexer(f))
            elif mem == 'best':
                subsystem = model_memory
            fixup_model(m)
            db.load(m)
            name = str(subsystem)
            time = db.get_result(name)
            if not time:
                m.memory = subsystem
                time = evaluate(m, directory)
                db.add_result(name, time)
            print(experiment + ',' + mem_model + ',' + str(time))


def main():
    options, args = parser.parse_args()
    if len(args) == 0:
        print('ERROR: no models specified')
        sys.exit(-1)
    if not database.get_instance(options.url):
        print('ERROR: could not connect to the database')
        sys.exit(-1)
    directory = options.directory if options.directory else os.getcwd()
    if options.compare:
        generate_matrix(args, options.memory, options.baseline, directory)
    else:
        generate_array(args, options.memory, options.baseline, directory)


if __name__ == '__main__':
    main()
