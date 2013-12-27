
from __future__ import print_function
import optparse
import os
import sys
import StringIO

from memsim import database, lex, memory, model
from memsim.util import get_experiment_name
from memsim.process import evaluate
from memsim.memory.main import MainMemory


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
parser.add_option('-r', '--replace', dest='replace', default=None,
                  help='file containing an alternate main memory')


def get_best(db, mod):
    best_name, _, _ = db.get_best(mod)
    best_file = StringIO.StringIO(best_name)
    return memory.parse_memory(lex.Lexer(best_file))


def get_memory(db, mem, mod, baseline, replace):
    """Get the specified memory.
    mem is the name of the memory to get.
    mod is the model.
    baseline is the name of the file containing a baseline memory.
    replace is the memory to use as the main memory (or None).
    Returns the memory to simulate.
    """
    if mem == 'model':
        # Use the subsystem from the model.
        subsystem = mod.memory
    elif mem == 'baseline':
        # Use the baseline subsystem.
        with open(baseline, 'r') as f:
            subsystem = memory.parse_memory(lex.Lexer(f))
    elif mem == 'best':
        # Use the best subsystem.
        subsystem = get_best(db, mod)
    else:
        print('ERROR: invalid memory selected:', mem)
        sys.exit(-1)

    if replace:
        with open(replace, 'r') as f:
            mod.memory = memory.parse_memory(lex.Lexer(f))
        ptr = subsystem
        while not isinstance(ptr.get_next(), MainMemory):
            ptr = ptr.get_next()
        ptr.set_next(mod.memory)

    return subsystem


def simulate(experiment, mem, baseline, replace, directory):
    m = model.parse_model_file(experiment)
    db = database.get_instance()
    subsystem = get_memory(db, mem, m, baseline, replace)
    fixup_model(m)
    time = db.get_result(m, subsystem)
    if time is None:
        m.memory = subsystem
        time, cost = evaluate(m, directory)
        db.add_result(m, subsystem, time, cost)
    print(get_experiment_name(experiment) + ',' + str(time))


def fixup_model(m):
    m.skip = 0
    m.on = 1000000


def generate_array(experiments, mem, baseline, replace, directory):
    for experiment in experiments:
        simulate(experiment, mem, baseline, replace, directory)


def generate_matrix(experiments, mem, baseline, replace, directory):
    assert(mem == 'best')
    db = database.get_instance()
    for mem_model in experiments:
        m = model.parse_model_file(mem_model)
        best_name, _, _ = db.get_best(m)
        best_file = StringIO.StringIO(best_name)
        model_memory = memory.parse_memory(lex.Lexer(best_file))
        for experiment in experiments:
            m = model.parse_model_file(experiment)
            subsystem = model_memory
            fixup_model(m)
            name = str(subsystem)
            time = db.get_result(m, name)
            if not time:
                m.memory = subsystem
                time, cost = evaluate(m, directory)
                db.add_result(m, name, time, cost)
            print(get_experiment_name(experiment) + ',' +
                  get_experiment_name(mem_model) + ',' + str(time))


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
        generate_matrix(args, options.memory, options.baseline,
                        options.replace, directory)
    else:
        generate_array(args, options.memory, options.baseline,
                       options.replace, directory)


if __name__ == '__main__':
    main()
