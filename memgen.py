
import optparse
import sys

from memsim import model, vhdl


parser = optparse.OptionParser()
parser.add_option('-m', '--model', dest='model', default=None,
                  help='file containing the memory subsystem specification')


def main():
    options, args = parser.parse_args()
    if not options.model:
        print('ERROR: no model file specified')
        sys.exit(-1)
    m = model.parse_model_file(options.model)
    if not m:
        print('ERROR: could not read model')
        sys.exit(-1)
    gen = vhdl.VHDLGenerator()
    result = gen.generate(m.machine, m.memory)
    print(result)


if __name__ == '__main__':
    main()
