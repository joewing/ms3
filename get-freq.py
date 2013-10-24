

import optparse
import sys

import lex
import model
import xilinx

parser = optparse.OptionParser()
parser.add_option('-m', '--model', dest='model', default=None,
                  help='the model to use')

def main():
   (options, args) = parser.parse_args()
   if options.model == None:
      print("ERROR: no model specified")
      sys.exit(-1)
   with open(options.model) as f:
      m = model.parse_model(lex.Lexer(f))
      frequency = xilinx.get_frequency(m.machine, m.memory)
      bram_count = xilinx.get_bram_count(m.machine, m.memory)
      print("Frequency: " + str(frequency))
      print("BRAMs:     " + str(bram_count))

if __name__ == '__main__':
   main()

