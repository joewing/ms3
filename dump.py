
import optparse
import os

import database

parser = optparse.OptionParser()
parser.add_option('-u', '--url', dest='url', default=None,
                        help='database URL')
parser.add_option('-r', '--remove', dest='remove', default=None,
                        help='remove a model by id')
parser.add_option('-c', '--compact', dest='compact', default=False,
                        action="store_true",
                        help='perform compaction')
parser.add_option('-f', '--fpga', dest='fpga', default=False,
                        action="store_true",
                        help='remove invalid FPGA results')


def show_state(db, state):
    if 'model' not in state:
        return
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
    if options.url is None:
        url = os.environ.get('COUCHDB_URL')
    else:
        url = options.url
    db = database.get_instance('', url)
    if options.fpga:
        db.remove_fpga()
    if options.compact:
        db.compact()
    if options.remove is not None:
        db.remove(options.remove)
    else:
        for state in db.get_states():
            show_state(db, state)
            print

if __name__ == '__main__':
    main()
