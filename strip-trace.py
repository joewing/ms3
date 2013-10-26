
import optparse
import sys
import process
from benchmarks.trace import Trace

parser = optparse.OptionParser()
parser.add_option('-t', '--trace', dest='trace', default=None,
                        help='the trace to process')
parser.add_option('-k', '--skip', dest='skip', default=100000,
                        help='number of requests to skip')
parser.add_option('-o', '--on', dest='on', default=10000,
                        help='number of requests to process between skips')


def main():
    (options, args) = parser.parse_args()
    if options.trace is None:
        print('ERROR: no trace specified')
        sys.exit(-1)
    trace = Trace(options.trace)
    on = int(options.on)
    skip = int(options.skip)
    count = 0
    for at, addr, size in trace.run():
        if count < on:
            if at == process.AccessType.READ:
                print('R' + hex(addr)[2:] + ":" + hex(size)[2:])
            elif at == process.AccessType.WRITE:
                print('W' + hex(addr)[2:] + ":" + hex(size)[2:])
            elif at == process.AccessType.IDLE:
                print('I' + hex(addr)[2:] + ":" + hex(size)[2:])
            elif at == process.AccessType.CONSUME:
                print('C' + hex(addr)[2:] + ":" + hex(size)[2:])
            elif at == process.AccessType.PRODUCE:
                print('P' + hex(addr)[2:] + ":" + hex(size)[2:])
            elif at == process.AccessType.END:
                print('X' + hex(addr)[2:] + ":" + hex(size)[2:])
            else:
                assert(False)
        count += 1
        if count == on + skip:
            count = 0

if __name__ == '__main__':
    main()
