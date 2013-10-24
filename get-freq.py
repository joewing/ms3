

import optparse
import sys

import lex
import model
import xilinx

parser = optparse.OptionParser()
parser.add_option('-m', '--model', dest='model', default=None,
                  help='the model to use')
parser.add_option('-k', '--keep', dest='keep', default=False,
                  action='store_true',
                  help='keep intermediate files')

def main():
   (options, args) = parser.parse_args()
   if options.model == None:
      print("ERROR: no model specified")
      sys.exit(-1)
   with open(options.model) as f:
      m = model.parse_model(lex.Lexer(f))
      result = xilinx.run_xilinx(m.machine, m.memory, options.keep)
      print("Frequency: " + str(result.frequency))
      print("BRAMs:     " + str(result.bram_count))

if __name__ == '__main__':
   main()

