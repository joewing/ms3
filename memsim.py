
import gc
import sys

import distribution
import lex
import memory
import model
import optimizer
import process

_defaults = {
   '-max_cost'    : 100000,
   '-seed'        : 7,
   '-iterations'  : 10000,
   '-model'       : 'model.txt'
}

def show_options(options):
   for k in sorted(options.keys()):
      d = options[k]
      print("   " + k + " " + str(d))

def show_usage():
   print("usage: memsim <options>")
   print("options:")
   show_options(_defaults)

def parse_options():
   options = _defaults
   argc = len(sys.argv)
   i = 1
   while i < argc:
      opt = sys.argv[i]
      if (i + 1 < argc) and (opt in options):
         value = sys.argv[i + 1]
         if isinstance(options[opt], int):
            options[opt] = int(value)
         else:
            options[opt] = value
         i += 2
      else:
         show_usage()
         sys.exit(-1)
   return options

def parse_file(options):
   try:
      with open(options['-model'], 'r') as f:
         return model.parse_model(lex.Lexer(f))
   except IOError:
      print("ERROR: could not open model")
      sys.exit(-1)

def main():

   options = parse_options()
   show_options(options)
   max_cost = options['-max_cost']
   seed = options['-seed']
   iterations = options['-iterations']
   machine, mem, benchmarks = parse_file(options)

   distributions = []
   processes = []
   memories = []
   for b in benchmarks:
      dist = distribution.Distribution(seed)
      distributions.append(dist)
      processes.append(process.Process(dist, b))
      memories.append(mem)

   pl = process.ProcessList(machine, processes)
   ml = memory.MemoryList(memories, distributions)
   o = optimizer.Optimizer(machine, ml,
                           max_cost = max_cost,
                           seed = seed,
                           iterations = iterations)
   time = pl.run(ml, True)
   while True:
      ml = o.optimize(time)
      if ml != None:
         time = pl.run(ml)
      else:
         break
      gc.collect()

if __name__ == '__main__':
   main()

