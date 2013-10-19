
import gc
import optparse
import sys

import database.couch
import database.simple
import distribution
import lex
import memory
import model
import optimizer
import process

parser = optparse.OptionParser()
parser.add_option('-d', '--db', dest='database', default=None,
                  help='database to use this run')
parser.add_option('-e', '--experiment', dest='experiment', default=None,
                  help='experiment to run')
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
   if options.experiment != None:
      options.database = options.experiment
      options.model = 'experiments/' + options.experiment

   m = parse_model_file(options.model)
   db = database.couch.CouchDatabase(m)
   if db.load():
      print("Connected to database")
   else:
      db = database.simple.SimpleDatabase(m)
      print("Could not connect to database")
   print(m)

   distributions = []
   processes = []
   memories = []
   for i in range(len(m.benchmarks)):
      dist = distribution.Distribution(m.seed)
      dist.load(i, db)
      distributions.append(dist)
      processes.append(process.Process(dist, m.benchmarks[i]))
      memories.append(m.memory)

   pl = process.ProcessList(m.machine, processes, db, m.on, m.skip)
   ml = memory.MemoryList(memories, distributions)
   o = optimizer.Optimizer(m.machine, ml, db, m.seed,
                           use_prefetch = pl.has_delay())
   ml = o.load()
   for _ in range(int(options.iterations)):
      time = pl.run(ml)
      ml = o.optimize(time, db)
      db.save()
      gc.collect()

if __name__ == '__main__':
   main()

