
from __future__ import print_function
import optparse
import os
import sys
import StringIO

from memsim import database, lex, memory, model
from memsim.util import get_experiment_name
from memsim.sim import evaluate
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
    return memory.parse_memory_list(lex.Lexer(best_file), mod.memory)


def get_memory_list(db, mem, mod, baseline, replace):
    """Get the specified memory.
    mem is the name of the memory to get.
    mod is the model.
    baseline is the name of the file containing a baseline memory.
    replace is the memory to use as the main memory (or None).
    Returns the memory to simulate.
    """
    if mem == 'model':
        # Use the subsystem from the model.
        ml = memory.MemoryList([mod.memory for _ in mod.benchmarks])
    elif mem == 'baseline':
        # Use the baseline subsystem.
        with open(baseline, 'r') as f:
            main_mem = memory.parse_memory(lex.Lexer(f))
        ml = memory.MemoryList([main_mem for _ in mod.benchmarks])
    elif mem == 'best':
        # Use the best subsystem.
        ml = get_best(db, mod)
    else:
        print('ERROR: invalid memory selected:', mem)
        sys.exit(-1)

    if replace:
        with open(replace, 'r') as f:
            mod.memory = memory.parse_memory(lex.Lexer(f))
        for m in ml.memories:
            ptr = m
            while not isinstance(ptr.get_next(), MainMemory):
                ptr = ptr.get_next()
            ptr.set_next(mod.memory)

    return ml


def simulate(experiment, mem, baseline, replace, directory):
    mod = model.parse_model_file(experiment)
    db = database.get_instance()
    ml = get_memory_list(db, mem, mod, baseline, replace)
    time = db.get_result(mod, ml)
    if time is None:
        time, cost = evaluate(mod, ml, directory)
        db.add_result(mod, ml, time, cost)
    print(get_experiment_name(experiment) + ',' + str(time))


def generate_array(experiments, mem, baseline, replace, directory):
    for experiment in experiments:
        simulate(experiment, mem, baseline, replace, directory)


def generate_matrix(experiments, mem, baseline, replace, directory):
    assert(mem == 'best')
    db = database.get_instance()
    for mem_model in experiments:
        mod = model.parse_model_file(mem_model)
        model_ml = get_best(db, mod)
        for experiment in experiments:
            mod = model.parse_model_file(experiment)
            time = db.get_result(mod, model_ml)
            if not time:
                time, cost = evaluate(mod, model_ml, directory)
                db.add_result(mod, model_ml, time, cost)
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
