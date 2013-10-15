
import optparse
import sys

import database

parser = optparse.OptionParser()
parser.add_option('-d', '--db', dest='database', default=None,
                  help='database to dump')

def show_database(db, name):
   print(name.name + ":")
   db.load(name)
   if not db.has_value('valid'):
      print("  <empty>")
      return
   instance = db.get_instance()
   if instance == None: instance = "<none>"
   iterations = db.get_value('evaluations')
   best_name = db.get_value('best_name')
   best_value = db.get_value('best_value')
   best_cost = db.get_value('best_cost')
   print("  Inst: " + instance)
   print("  Iter: " + str(iterations))
   print("  Best: " + best_name)
   print("  Time: " + str(best_value))
   print("  Cost: " + str(best_cost))

def main():
   (options, args) = parser.parse_args()
   db = database.Database()
   if options.database != None:
      show_database(db, options.database)
   else:
      for n in db.list():
         show_database(db, n)
         print

if __name__ == '__main__':
   main()

