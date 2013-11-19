
from __future__ import print_function
import math
import optparse
import os

from memsim import database, machine, memory, model, process
from memsim.memory import cache, ram


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
parser.add_option('-d', '--directory', dest='directory', default='',
                  help='trace directory')


total = 0
mach = None
best_name = ''
best_cost = 0
best_time = 1 << 31
max_cost = DEFAULT_MAX_COST
directory = ''
url = ''


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
    m = model.parse_model_file(experiment)
    procs = [process.Process(None, m.benchmarks[0])]
    pl = process.ProcessList(mach, procs, directory, 1000000, 0)
    pl.first = False
    mem.set_next(m.memory)
    m.memory = mem
    m.skip = 0
    m.on = 1000000
    db = database.get_instance(m, url)
    result = db.get_result(mem)
    if not result:
        ml = memory.MemoryList([mem], [None])
        result = pl.run(ml, 0)
        db.add_result(mem, result)
    return result


def run_simulations(mem, experiments):
    global best_time, best_cost, best_name
    print("Evaluating", mem)
    if experiments is None:
        global total
        print('  Total:', str(total))
        return
    lsum = 0.0
    gmean = 0.0
    for e in experiments:
        result = run_simulation(mem, e)
        lsum += math.log(result)
        gmean = math.exp(lsum / len(experiments))
        if gmean > best_time:
            print('  Best cost exceeded')
            return
    cost = mem.get_cost()
    if gmean < best_time or (gmean == best_time and cost < best_cost):
        print('  New best:', gmean)
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
        if best_name:
            print("Best:", best_name)


def get_policies(associativity):
    if associativity == 1:
        return [0]
    else:
        return range(0, cache.CachePolicy.MAX_POLICY + 1)


def main():
    global url, directory, max_cost, mach
    options, args = parser.parse_args()
    experiments = args if args else None
    url = options.url if options.url else os.environ.get('COUCHDB_URL')
    directory = options.directory
    database.get_instance('', url)
    target = machine.parse_target(options.target)
    mach = machine.MachineType(target=target,
                               frequency=float(options.frequency),
                               word_size=int(options.word_size),
                               addr_bits=32,
                               max_path_length=64)
    max_cost = int(options.cost)
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
