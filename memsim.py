
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
parser.add_option('-d', '--db', dest='database', default=None,
                  help='database to use this run')
parser.add_option('-e', '--experiment', dest='experiment', default=None,
                  help='experiment to run')
parser.add_option('-s', '--seed', dest='seed', default=7,
                  help='random number seed for the optimizer')
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

   mach, mem, bms = parse_model_file(options.model)
   if options.database != None:
      print("Database: " + options.database)
      db = database.Database()
      db.load(options.database)
      db_valid = db.has_value('valid')
   else:
      print("Database: <none>")
      db = None
      db_valid = False
   print(mach)

   distributions = []
   processes = []
   memories = []
   for i in range(len(bms)):
      dist = distribution.Distribution(options.seed)
      if db_valid: dist.load(i, db)
      distributions.append(dist)
      processes.append(process.Process(dist, bms[i]))
      memories.append(mem)

   pl = process.ProcessList(mach, processes, not db_valid)
   ml = memory.MemoryList(memories, distributions)
   o = optimizer.Optimizer(mach, ml,
                           seed = int(options.seed),
                           use_prefetch = pl.has_delay())
   if db_valid: ml = o.load(db)
   for _ in range(int(options.iterations)):
      time = pl.run(ml)
      ml = o.optimize(time)
      gc.collect()
   if db != None:
      for i in range(len(bms)):
         distributions[i].save(i, db)
      o.save(db)
      db.set_value('valid', True)
      db.save()

if __name__ == '__main__':
   main()

