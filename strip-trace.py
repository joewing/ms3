
from __future__ import print_function
import collections
import optparse
import sys

from memsim import process
from memsim.benchmarks.trace import Trace

parser = optparse.OptionParser()
parser.add_option('-t', '--trace', dest='trace', default=None,
                  help='the trace to process')
parser.add_option('-k', '--skip', dest='skip', default=100000,
                  help='number of requests to skip')
parser.add_option('-o', '--on', dest='on', default=10000,
                  help='number of requests to process between skips')
parser.add_option('-w', '--word_size', dest='word_size', default=4,
                  help='word size in bytes')
parser.add_option('-c', '--min_count', dest='min_count', default=0,
                  help='minimum number of accesses to each memory location')


class TraceData:

    def __init__(self, word_size, min_count):
        self.word_size = word_size
        self.min_count = min_count
        self.min_address = sys.maxint
        self.max_address = 0
        self.addresses = collections.defaultdict(lambda: 0)
        self.has_data = False

    def insert_access(self, address, size):
        base_addr = address // self.word_size
        word_count = (size + self.word_size - 1) // self.word_size
        for offset in range(word_count):
            addr = base_addr + offset
            self.addresses[addr] += 1

    def should_output(self, address, size):
        if self.min_count > 0:
            base_addr = address // self.word_size
            word_count = (size + self.word_size - 1) // self.word_size
            for offset in range(word_count):
                if self.addresses[base_addr + offset] >= self.min_count:
                    return True
            return False
        else:
            return True


def collect_stats(trace, data):
    for at, addr, size in trace.run():
        if at == process.AccessType.READ:
            data.insert_access(addr, size)
        elif at == process.AccessType.WRITE:
            data.insert_access(addr, size)


def output_trace(trace, trace_data, on, skip):
    count = 0
    for at, addr, size in trace.run():
        if at == process.AccessType.READ:
            output = ['R', hex(addr)[2:], ":", hex(size)[2:]]
        elif at == process.AccessType.WRITE:
            output = ['W', hex(addr)[2:], ":", hex(size)[2:]]
        elif at == process.AccessType.IDLE:
            output = ['I', hex(addr)[2:], ":", hex(size)[2:]]
        elif at == process.AccessType.CONSUME:
            output = ['C', hex(addr)[2:], ":", hex(size)[2:]]
        elif at == process.AccessType.PRODUCE:
            output = ['P', hex(addr)[2:], ":", hex(size)[2:]]
        elif at == process.AccessType.END:
            output = ['X', hex(addr)[2:], ":", hex(size)[2:]]
        else:
            assert(False)
        if count < on and trace_data.should_output(addr, size):
            print(output)
        count += 1
        if count == on + skip:
            count = 0


def main():
    options, args = parser.parse_args()
    if options.trace is None:
        print('ERROR: no trace specified')
        sys.exit(-1)
    trace = Trace(options.trace)
    on = int(options.on)
    skip = int(options.skip)
    min_count = int(options.min_count)
    word_size = int(options.word_size)
    data = TraceData(word_size, min_count)
    if min_count > 0:
        collect_stats(trace, data)
        output_trace(trace, data, on, skip)
    else:
        output_trace(trace, data, on, skip)


if __name__ == '__main__':
    main()
