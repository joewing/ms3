
import optparse
import sys

import database.couch

parser = optparse.OptionParser()
parser.add_option('-r', '--remove', dest='remove', default=None,
                  help='remove a model by id')
parser.add_option('-c', '--compact', dest='compact', default=False,
                  action="store_true",
                  help='perform compaction')

def show_state(db, state):
   if 'model' not in state: return
   model = state['model']
   print(model)
   iterations = state['evaluations']
   best_name = state['best_name']
   best_value = state['best_value']
   best_cost = state['best_cost']
   print("  Hash: " + db.get_hash(model))
   print("  Iter: " + str(iterations))
   print("  Best: " + best_name)
   print("  Time: " + str(best_value))
   print("  Cost: " + str(best_cost))

def main():
   (options, args) = parser.parse_args()
   db = database.couch.CouchDatabase()
   db.load()
   if options.compact:
      db.compact()
   if options.remove != None:
      db.remove(options.remove)
   else:
      for state in db.get_states():
         show_state(db, state)
         print

if __name__ == '__main__':
   main()

