
from __future__ import print_function
import optparse
import sys

from memsim import model
from memsim.memory import xilinx


parser = optparse.OptionParser()
parser.add_option('-m', '--model', dest='model', default=None,
                        help='the model to use')
parser.add_option('-k', '--keep', dest='keep', default=False,
                        action='store_true',
                        help='keep intermediate files')


def main():
    options, args = parser.parse_args()
    if options.model is None:
        print('ERROR: no model specified')
        sys.exit(-1)
    m = model.parse_model_file(options.model)
    if not m:
        print('ERROR: could not read model')
        sys.exit(-1)
    m.machine.frequency = 1 << 31
    result = xilinx.run_xilinx(m.machine, m.memory, options.keep)
    print("Frequency:", result.frequency)
    print("BRAMs:     ", result.bram_count)


if __name__ == '__main__':
    main()
