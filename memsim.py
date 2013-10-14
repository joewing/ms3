
import gc
import optparse
import sys

import distribution
import lex
import memory
import model
import optimizer
import process

parser = optparse.OptionParser()
parser.add_option('-c', '--max_cost', dest='max_cost', default=100000,
                  help='max subsystem cost')
parser.add_option('-s', '--seed', dest='seed', default=7,
                  help='random number seed for the optimizer')
parser.add_option('-i', '--iterations', dest='iterations', default=10000,
                  help='number of iterations for optimization')
parser.add_option('-m', '--model', dest='model', default='model.txt',
                  help='model to use for optimization')

def parse_model_file(file_name):
   try:
      with open(file_name, 'r') as f:
         return model.parse_model(lex.Lexer(f))
   except IOError as e:
      print("ERROR: could not open model: " + str(e))
      sys.exit(-1)
   except lex.ParseError as e:
      print("ERROR: " + str(e))
      sys.exit(-1)

def main():

   (options, args) = parser.parse_args()
   mach, mem, bms = parse_model_file(options.model)

   print(mach)

   distributions = []
   processes = []
   memories = []
   for b in bms:
      dist = distribution.Distribution(options.seed)
      distributions.append(dist)
      processes.append(process.Process(dist, b))
      memories.append(mem)

   pl = process.ProcessList(mach, processes)
   ml = memory.MemoryList(memories, distributions)
   time = pl.run(ml)
   o = optimizer.Optimizer(mach, ml,
                           max_cost = int(options.max_cost),
                           seed = int(options.seed),
                           iterations = int(options.iterations),
                           use_prefetch = pl.has_delay())
   if all(map(lambda d: d.is_empty(), distributions)):
      print("ERROR: no valid address trace for optimization")
      sys.exit(-1)
   while True:
      ml = o.optimize(time)
      if ml != None:
         time = pl.run(ml)
      else:
         break
      gc.collect()

if __name__ == '__main__':
   main()

