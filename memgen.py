from __future__ import print_function
import optparse
import sys

from memsim import model, vhdl


def main():
    parser = optparse.OptionParser()
    options, args = parser.parse_args()
    if len(args) < 1:
        print('ERROR: no model file specified')
        sys.exit(-1)
    elif len(args) > 1:
        print('ERROR: too many options')
        sys.exit(-1)
    m = model.parse_model_file(args[0])
    gen = vhdl.VHDLGenerator(m.machine)
    result = gen.generate(m.memory)
    print(result)


if __name__ == '__main__':
    main()
