
from __future__ import print_function
import optparse
import os

from memsim import database, lex, model


parser = optparse.OptionParser()
parser.add_option('-u', '--url', dest='url', default=None,
                  help='database URL')
parser.add_option('-s', '--show', default=False, action='store_true',
                  help='show results')
parser.add_option('-p', '--pending', default=False, action='store_true',
                  help='show pending experiments')


def get_name_map(db):
    names = dict()
    for name in os.listdir('experiments'):
        try:
            with open('experiments/' + name, 'r') as f:
                m = model.parse_model(lex.Lexer(f))
                key = db.get_hash(m)
                names[key] = name
        except:
            pass
    return names


def show_state(db):
    names = get_name_map(db)
    for mname, evals, value in db.get_status():
        key = db.get_hash(mname)
        if key in names:
            print(mname)
            key = db.get_hash(mname)
            db.load(mname)
            best_name, best_value, best_cost = db.get_best()
            print('  Hash:', key)
            print('  Iter:', evals)
            print('  Best:', best_name)
            print('  Time:', best_value)
            print('  Cost:', best_cost)
            print()


def show_pending(db):
    names = get_name_map(db)
    for mname, evals, value in db.get_status():
        key = db.get_hash(mname)
        if key in names:
            name = names[key]
            pad1 = max(20 - len(name), 0)
            pad2 = max(8 - len(str(evals)), 0)
            print("{}: {}{}{}{}"
                  .format(name, " " * pad1,
                          evals, " " * pad2,
                          value))


def main():
    options, args = parser.parse_args()
    db = database.get_instance(options.url)
    if options.show:
        show_state(db)
    if options.pending:
        show_pending(db)


if __name__ == '__main__':
    main()
