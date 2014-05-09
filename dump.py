from __future__ import print_function
import optparse

from memsim import database, lex, model


parser = optparse.OptionParser()
parser.add_option('-u', '--url', dest='url', default=None,
                  help='database URL')
parser.add_option('-s', '--show', default=False, action='store_true',
                  help='show results')
parser.add_option('-m', '--memory', default=False, action='store_true',
                  help='dump memory spec')


def print_print_ml(ml):
    s = str(ml)
    return s.replace(') (', ')\n        (')


def get_name_map(db, experiments):
    names = dict()
    for name in experiments:
        try:
            with open(name, 'r') as f:
                m = model.parse_model(lex.Lexer(f))
                key = db.get_hash(m)
                names[key] = name
        except:
            pass
    return names


def show_state(db, experiments):
    names = get_name_map(db, experiments)
    for mname, evals, value in db.get_status():
        key = db.get_hash(mname)
        if key in names:
            print(mname)
            best_name, best_value, best_cost = db.get_best(mname)
            print('  Hash: ', db.get_hash(mname))
            print('  Iter: ', evals)
            print('  Best: ', print_print_ml(best_name))
            print('  Value:', best_value)
            print('  Cost: ', best_cost)
            print()


def dump_spec(db, experiments):
    names = get_name_map(db, experiments)
    for mname, _, _ in db.get_status():
        key = db.get_hash(mname)
        if key in names:
            best_name, _, _ = db.get_best(mname)
            print(mname)
            print('(memory')
            print(best_name)
            print(')')


def show_pending(db, experiments):
    names = get_name_map(db, experiments)
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
        show_state(db, args)
    elif options.memory:
        dump_spec(db, args)
    else:
        show_pending(db, args)


if __name__ == '__main__':
    main()
