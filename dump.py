
import optparse
import sys

import database

parser = optparse.OptionParser()
parser.add_option('-d', '--db', dest='database', default=None,
                  help='database to dump')

def main():
   (options, args) = parser.parse_args()
   if options.database == None:
      print("ERROR: no database specified")
      sys.exit(-1)

   db = database.Database(options.database)
   if not db.has_value('valid'):
      print("ERROR: database empty")
      sys.exit(-1)

   iterations = db.get_value('evaluations')
   best_name = db.get_value('best_name')
   best_value = db.get_value('best_value')
   best_cost = db.get_value('best_cost')
   print("Iterations: " + str(iterations))
   print("Best: " + best_name)
   print("Time: " + str(best_value))
   print("Cost: " + str(best_cost))

if __name__ == '__main__':
   main()
