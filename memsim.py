
import sys
from Memory.Memory import MemoryList
import Distribution
import Process
import Optimizer
import Machine
import Benchmark.Hash
import Benchmark.MM
import Lexer
import TopParser

_defaults = {
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
      model = options['-model']
      with open(model, 'r') as f:
         lexer = Lexer.Lexer(f)
         return TopParser.parse(lexer)
   except IOError:
      print("ERROR: could not open model")
      sys.exit(-1)

def main():

   options = parse_options()
   show_options(options)
   seed = options['-seed']
   iterations = options['-iterations']
   machine, memory, benchmarks = parse_file(options)

   distributions = []
   processes = []
   memories = []
   for b in benchmarks:
      dist = Distribution.Distribution(seed)
      distributions.append(dist)
      processes.append(Process.Process(dist, b))
      memories.append(memory)

   pl = Process.ProcessList(machine, processes)
   ml = MemoryList(memories, distributions)
   o = Optimizer.Optimizer(machine, ml, seed = seed, iterations = iterations)
   time = pl.run(ml, True)
   while True:
      ml = o.optimize(time)
      if ml != None:
         time = pl.run(ml)
      else:
         break

if __name__ == '__main__':
   main()

