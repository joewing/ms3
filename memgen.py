
import optparse
import sys

import lex
import model
import vhdl

parser = optparse.OptionParser()
parser.add_option('-m', '--model', dest='model', default=None,
                  help='file containing the memory subsystem specification')

def generate(f):
   lexer = lex.Lexer(f)
   m = model.parse_model(lexer)
   gen = vhdl.VHDLGenerator()
   result = gen.generate(m.machine, m.memory)
   print(result)

def main():
   (options, args) = parser.parse_args()
   if options.model != None:
      with open(options.model) as f:
         generate(f)
   else:
      print("ERROR: no model file specified")
      sys.exit(-1)

if __name__ == '__main__':
   main()
