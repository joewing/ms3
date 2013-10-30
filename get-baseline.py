
from __future__ import print_function
import math
import optparse
import os
import sys

from memsim import database
from memsim import distribution
from memsim import lex
from memsim import machine
from memsim import memory
from memsim.memory import cache
from memsim.memory import ram
from memsim import model
from memsim import process


parser = optparse.OptionParser()
parser.add_option('-u', '--url', dest='url', default=None,
                  help='database URL')


db = None
total = 0
mach = machine.MachineType(target=machine.TargetType.FPGA,
                           frequency=250000000,
                           word_size=4,
                           addr_bits=32,
                           max_path_length=64)
best_name = ''
best_cost = 0
best_time = 1 << 31


def run_simulation(mem, experiment):
    print("  Running", experiment)
    with open(experiment, 'r') as f:
        l = lex.Lexer(f)
        m = model.parse_model(l)
    dist = distribution.Distribution(m.seed)
    procs = [process.Process(dist, m.benchmarks[0])]
    pl = process.ProcessList(mach, procs, 1000000, 0)
    pl.first = False
    mem.set_next(m.memory)
    ml = memory.MemoryList([mem], [dist])
    return pl.run(ml, 0)


def run_simulations(mem, experiments):
    print("Evaluating", mem)
    lsum = 0.0
    for e in experiments:
        result = run_simulation(mem, e)
        lsum += math.log(result)
    gmean = math.exp(lsum / len(experiments))
    print("Geometric mean:", gmean)
    cost = mem.get_cost()
    global best_time, best_cost, best_name
    if gmean < best_time or \
       (gmean == best_time and cost < best_cost):
        best_time = gmean
        best_cost = cost
        best_name = str(mem)
    return gmean


def generate_cache(line_count,
                   line_size,
                   associativity,
                   policy,
                   write_back,
                   experiments):
    c = cache.Cache(mem=ram.RAM(latency=0),
                    line_count=line_count,
                    line_size=line_size,
                    associativity=associativity,
                    policy=policy,
                    write_back=write_back)
    c.reset(mach)
    cost = c.get_cost()
    if cost <= 64:
        global total
        total += 1
        run_simulations(c, experiments)


def get_policies(associativity):
    if associativity == 1:
        return [0]
    else:
        return range(0, cache.CachePolicy.MAX_POLICY)


def main():
    (options, args) = parser.parse_args()
    if len(args) == 0:
        print("no experiments provided")
        sys.exit(0)
    experiments = args
    if options.url is None:
        url = os.environ.get('COUCHDB_URL')
    else:
        url = options.url
    global db
    db = database.get_instance('', url)
    max_brams = 64
    bram_size = 512 * 72 / 8
    word_size = 4
    line_count = 256
    while line_count <= max_brams * bram_size // word_size:
        line_size = word_size
        while line_size * line_count <= max_brams * bram_size:
            associativity = 1
            while associativity <= min(line_count, 8):
                for policy in get_policies(associativity):
                    generate_cache(line_count, line_size, associativity,
                                   policy, True, experiments)
                    generate_cache(line_count, line_size, associativity,
                                   policy, False, experiments)
                associativity *= 2
            line_size *= 2
        line_count *= 2
    print("Total:", total)
    print("Best Cost:  ", best_cost)
    print("Best Memory:", best_name)


if __name__ == '__main__':
    main()
