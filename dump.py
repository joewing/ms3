
import optparse
import sys

import database.couch

parser = optparse.OptionParser()
parser.add_option('-d', '--db', dest='database', default=None,
                  help='database to dump')

def show_state(state):
   if 'model' not in state: return
   model = state['model']
   print(model)
   iterations = state['evaluations']
   best_name = state['best_name']
   best_value = state['best_value']
   best_cost = state['best_cost']
   print("  Iter: " + str(iterations))
   print("  Best: " + best_name)
   print("  Time: " + str(best_value))
   print("  Cost: " + str(best_cost))

def main():
   (options, args) = parser.parse_args()
   db = database.couch.CouchDatabase()
   db.load()
   for state in db.get_states():
      show_state(state)

if __name__ == '__main__':
   main()

