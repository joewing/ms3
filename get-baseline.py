
from __future__ import print_function
import math
import optparse
import os

from memsim import database
from memsim import distribution
from memsim import lex
from memsim import machine
from memsim import memory
from memsim.memory import cache
from memsim.memory import ram
from memsim import model
from memsim import process


BRAM_WIDTH = 72
BRAM_DEPTH = 512
DEFAULT_MAX_COST = 64
DEFAULT_WORD_SIZE = 4

parser = optparse.OptionParser()
parser.add_option('-u', '--url', dest='url', default=None,
                  help='database URL')
parser.add_option('-c', '--cost', dest='cost', default=DEFAULT_MAX_COST,
                  help='max cost')
parser.add_option('-w', '--word', dest='word_size', default=DEFAULT_WORD_SIZE,
                  help='word size in bytes')
parser.add_option('-t', '--target', dest='target', default='fpga',
                  help='target architecture')
parser.add_option('-f', '--frequency', dest='frequency', default=250000000,
                  help='target frequency')


db = None
total = 0
mach = None
best_name = ''
best_cost = 0
best_time = 1 << 31
max_cost = DEFAULT_MAX_COST


def estimate_cost(width, depth):
    if mach.target == machine.TargetType.FPGA:
        if width % BRAM_WIDTH != 0:
            max_width = BRAM_WIDTH * BRAM_DEPTH
            small_width = width % BRAM_WIDTH
            rounded_width = machine.round_power2(small_width)
            small_depth = max_width // rounded_width
            result = (depth + small_depth - 1) // small_depth
        else:
            result = 0
        big_count = width // BRAM_WIDTH
        big_depth = (depth + BRAM_DEPTH - 1) // BRAM_DEPTH
        result += big_depth * big_count
        return result
    elif mach.target == machine.TargetType.SIMPLE:
        return width * depth
    else:
        return width * depth * mach.technology


def get_max_size():
    if mach.target == machine.TargetType.FPGA:
        return max_cost * BRAM_WIDTH * BRAM_DEPTH
    elif mach.target == machine.TargetType.ASIC:
        return int(max_cost / mach.technology)
    else:
        return max_cost


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
    global best_time, best_cost, best_name
    print("Evaluating", mem)
    if experiments is None:
        global total
        print("  Total:", str(total))
        return
    lsum = 0.0
    gmean = 0.0
    for e in experiments:
        result = run_simulation(mem, e)
        lsum += math.log(result)
        gmean = math.exp(lsum / len(experiments))
        if gmean > best_time:
            print("Best cost exceeded")
            return
    cost = mem.get_cost()
    if gmean < best_time or (gmean == best_time and cost < best_cost):
        print("New best:", gmean)
        best_time = gmean
        best_cost = cost
        best_name = str(mem)


def generate_cache(line_count,
                   line_size,
                   associativity,
                   policy,
                   write_back,
                   experiments):
    width = line_size * associativity * 8
    depth = line_count // associativity
    if estimate_cost(width, depth) > max_cost:
        return
    c = cache.Cache(mem=ram.RAM(latency=0),
                    line_count=line_count,
                    line_size=line_size,
                    associativity=associativity,
                    policy=policy,
                    write_back=write_back)
    c.reset(mach)
    cost = c.get_cost()
    if cost <= max_cost:
        global total
        total += 1
        run_simulations(c, experiments)


def get_policies(associativity):
    if associativity == 1:
        return [0]
    else:
        return range(0, cache.CachePolicy.MAX_POLICY + 1)


def main():
    global db, max_cost, mach
    options, args = parser.parse_args()
    if len(args) == 0:
        experiments = None
    else:
        experiments = args
    if options.url is None:
        url = os.environ.get('COUCHDB_URL')
    else:
        url = options.url
    target = machine.parse_target(options.target)
    mach = machine.MachineType(target=target,
                               frequency=float(options.frequency),
                               word_size=int(options.word_size),
                               addr_bits=32,
                               max_path_length=64)
    max_cost = int(options.cost)
    db = database.get_instance('', url)
    max_size = get_max_size()
    line_count = machine.round_power2(max_size // (mach.word_size * 8))
    while line_count >= 128:
        line_size = machine.round_power2(max_size // 8)
        while line_size >= mach.word_size:
            associativity = min(line_count, 8)
            while associativity >= 1:
                for policy in get_policies(associativity):
                    generate_cache(line_count, line_size, associativity,
                                   policy, True, experiments)
                    generate_cache(line_count, line_size, associativity,
                                   policy, False, experiments)
                associativity //= 2
            line_size //= 2
        line_count //= 2
    print("Total:", total)
    print("Best Cost:  ", best_cost)
    print("Best Memory:", best_name)


if __name__ == '__main__':
    main()
