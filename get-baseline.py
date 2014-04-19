
from __future__ import print_function
import math
import optparse
import sys

from memsim import database, machine, memory, model, util
from memsim.sim import evaluate
from memsim.memory import cache, ram


#BRAM_WIDTH = 72
BRAM_WIDTH = 36
BRAM_DEPTH = 512

parser = optparse.OptionParser()
parser.add_option('-u', '--url', dest='url', default=None,
                  help='database URL')
parser.add_option('-d', '--directory', dest='directory', default='',
                  help='trace directory')


total = 0
mach = None
best_name = ''
best_cost = 0
best_time = 1 << 31
directory = ''


def estimate_cost(width, depth):
    if mach.target == machine.TargetType.FPGA:
        if width % BRAM_WIDTH != 0:
            max_width = BRAM_WIDTH * BRAM_DEPTH
            small_width = width % BRAM_WIDTH
            rounded_width = util.round_power2(small_width)
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
        return mach.max_cost * BRAM_WIDTH * BRAM_DEPTH
    elif mach.target == machine.TargetType.ASIC:
        return int(mach.max_cost / mach.technology)
    else:
        return mach.max_cost


def run_simulation(mem, experiment):
    print('  Running', experiment)
    m = model.parse_model_file(experiment)
    if m.machine.target != mach.target:
        print('ERROR: wrong target for', experiment)
        sys.exit(-1)
    if m.machine.frequency != mach.frequency:
        print('ERROR: wrong frequency for', experiment)
        sys.exit(-1)
    if m.machine.technology != mach.technology:
        print('ERROR: wrong technology for', experiment)
        sys.exit(-1)
    if m.machine.max_path_length != mach.max_path_length:
        print('ERROR: wrong max path length for', experiment)
        sys.exit(-1)
    if m.machine.part != mach.part:
        print('ERROR: wrong part for', experiment)
        sys.exit(-1)
    if m.machine.word_size != mach.word_size:
        print('ERROR: wrong word size for', experiment)
        sys.exit(-1)
    if m.machine.addr_bits != mach.addr_bits:
        print('ERROR: wrong addr bits for', experiment)
        sys.exit(-1)
    if m.machine.max_cost != mach.max_cost:
        print('ERROR: wrong max cost for', experiment)
        sys.exit(-1)
    mem.set_main(m.memory)
    db = database.get_instance()
    result = db.get_result(m, mem)
    if result is None:
        ml = memory.MemoryList(m.memory)
        ml.add_memory(mem)
        result, cost = evaluate(m, ml, directory)
        db.add_result(m, mem, result, cost)
    return result


def run_simulations(mem, experiments):
    global best_time, best_cost, best_name
    print('Evaluating', mem)
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
    if estimate_cost(width, depth) > mach.max_cost:
        return
    c = cache.Cache(mem=ram.RAM(latency=0),
                    line_count=line_count,
                    line_size=line_size,
                    associativity=associativity,
                    policy=policy,
                    write_back=write_back)
    c.reset(mach)
    cost = c.get_cost()
    if cost <= mach.max_cost:
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
    global directory, mach
    options, args = parser.parse_args()
    experiments = args if args else None
    if not database.get_instance(options.url):
        print('ERROR: could not connect to the database')
        sys.exit(-1)
    directory = options.directory
    if len(args) > 0:
        m = model.parse_model_file(args[0])
        mach = m.machine
    else:
        mach = machine.MachineType()
    max_size = get_max_size()
    line_count = util.round_power2(max_size // (mach.word_size * 8))
    while line_count >= 128:
        line_size = util.round_power2(max_size // 8)
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
    print('Total:', total)
    print('Best Cost:  ', best_cost)
    print('Best Memory:', best_name)


if __name__ == '__main__':
    main()
