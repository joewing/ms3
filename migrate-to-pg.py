
from __future__ import print_function
import optparse
import sys

from memsim.database.couch import CouchDatabase
from memsim.database.pg import PGDatabase


parser = optparse.OptionParser()
parser.add_option('-c', '--couch', dest='couch', default=None,
                  help='CouchDB URL')
parser.add_option('-p', '--pg', dest='pg', default=None,
                  help='PostgreSQL URL')


def migrate_models(couch, pg):
    print("Migrating models")


def migrate_fpga_results(couch, pg):
    print("Migrating FPGA results")
    for k, v, data in couch.get_fpga_results():
        print(k)
        freq = v[0]
        bram_count = v[1]
        pg.add_fpga_result(data, freq, bram_count)


def migrate_cacti_results(couch, pg):
    print("Migrating CACTI results")
    for k, v, data in couch.get_cacti_results():
        print(k)
        access_time = v[0]
        cycle_time = v[1]
        area = v[2]
        pg.add_cacti_result(data, access_time, cycle_time, area)


def main():

    options, args = parser.parse_args()
    pg_url = options.pg
    couch_url = options.couch
    if not pg_url:
        print('ERROR: PostgreSQL URL not provided')
        sys.exit(-1)
    if not couch_url:
        print('ERROR: CouchDB URL not provided')
        sys.exit(-1)

    # Connect to CouchDB.
    couch = CouchDatabase(couch_url)
    if not couch.connect():
        print('ERROR: could not connect to CouchDB')
        sys.exit(-1)
    print('Connected to CouchDB')

    # Connect to PostgreSQL.
    pg = PGDatabase(pg_url)
    if not pg.connect():
        print('ERROR: could not connect to PostgreSQL')
        sys.exit(-1)
    print('Connected to PostgreSQL')

    # Perform the migration.
    migrate_models(couch, pg)
    migrate_fpga_results(couch, pg)
    migrate_cacti_results(couch, pg)


if __name__ == '__main__':
    main()
