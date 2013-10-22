
import gc
import optparse
import sys

import database
import distribution
import lex
import memory
import model
import optimizer
import process

parser = optparse.OptionParser()
parser.add_option('-u', '--url', dest='url',
                  default='http://127.0.0.1:5984',
                  help='database URL')
parser.add_option('-i', '--iterations', dest='iterations', default=1,
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
   m = parse_model_file(options.model)
   db = database.get_instance(m, options.url)
   db.set_value('model', str(m))
   print(m)

   distributions = []
   processes = []
   memories = []
   for i in range(len(m.benchmarks)):
      dist = distribution.Distribution(m.seed)
      dist.load(i)
      distributions.append(dist)
      processes.append(process.Process(dist, m.benchmarks[i]))
      memories.append(m.memory)

   pl = process.ProcessList(m.machine, processes, m.on, m.skip)
   ml = memory.MemoryList(memories, distributions)
   o = optimizer.Optimizer(m.machine, ml, m.seed,
                           use_prefetch = pl.has_delay())
   ml = o.load()
   limit = o.best_value * 4
   while o.evaluations < int(options.iterations):
      time = pl.run(ml, limit)
      ml = o.optimize(time)
      db.save()
      gc.collect()
   else:
      print("Iterations:  " + str(o.evaluations))
      print("Best Memory: " + str(o.best_name))
      print("Best Value:  " + str(o.best_value))
      print("Best Cost:   " + str(o.best_cost))

if __name__ == '__main__':
   main()

