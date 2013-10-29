
import optparse
import os

import database
import lex
import model

parser = optparse.OptionParser()
parser.add_option('-u', '--url', dest='url', default=None,
                  help='database URL')
parser.add_option('-r', '--remove', dest='remove', default=None,
                  help='remove a model by id')
parser.add_option('-c', '--compact', dest='compact', default=False,
                  action='store_true',
                  help='perform compaction')
parser.add_option('-f', '--fpga', dest='fpga', default=False,
                  action='store_true',
                  help='remove invalid FPGA results')
parser.add_option('-s', '--show', default=False, action='store_true',
                  help='show results')
parser.add_option('-p', '--pending', default=False, action='store_true',
                  help='show pending experiments')


def show_state(db, state):
    if 'model' not in state:
        return
    m = state['model']
    print(m)
    iterations = state['evaluations']
    best_name = state['best_name']
    best_value = state['best_value']
    best_cost = state['best_cost']
    print("  Hash: " + db.get_hash(m))
    print("  Iter: " + str(iterations))
    print("  Best: " + best_name)
    print("  Time: " + str(best_value))
    print("  Cost: " + str(best_cost))


def show_pending(db):
    models = dict()
    for e in os.listdir('experiments'):
        try:
            with open('experiments/' + e, 'r') as f:
                l = lex.Lexer(f)
                m = model.parse_model(l)
                models[db.get_hash(m)] = e
        except:
            pass
    iterations = dict()
    for state in db.get_states():
        if 'model' in state:
            key = db.get_hash(state['model'])
            iterations[key] = state['evaluations']
    for key in models:
        name = models[key]
        i = iterations.get(key, 0)
        pad = max(16 - len(name), 0)
        print(name + ":" + (" " * pad) + str(i))


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
    if options.show:
        for state in db.get_states():
            show_state(db, state)
            print
    if options.pending:
        show_pending(db)

if __name__ == '__main__':
    main()
