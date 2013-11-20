
from __future__ import print_function
import optparse
import os

from memsim.database.couch import CouchDatabase
from memsim.database.s3 import S3Database


parser = optparse.OptionParser()
parser.add_option('-u', '--url', dest='url', default=None,
                  help='CouchDB URL')


def migrate_states(couch, s3):
    print("Migrating states")
    for state in couch.get_states():
        model_hash = s3.get_hash(state['model'])
        state_key = '-'.join(['state', model_hash])
        print(state_key)
        s3.put(state_key, state)
        for k, v in couch.get_results(model_hash):
            result_key = '-'.join(['result', model_hash, k])
            print(result_key)
            s3.put(result_key, v)


def migrate_fpga_results(couch, s3):
    print("Migrating FPGA results")
    for k, v, _ in couch.get_fpga_results():
        print(k)
        fpga_key = '-'.join(['fpga', k])
        s3.put(fpga_key, v)


def migrate_cacti_results(couch, s3):
    print("Migrating CACTI results")
    for k, v, _ in couch.get_cacti_results():
        print(k)
        cacti_key = '-'.join(['cacti', k])
        s3.put(cacti_key, v)


def main():

    options, args = parser.parse_args()
    url = options.url if options.url else os.environ.get('COUCHDB_URL')

    # Connect to CouchDB.
    couch = CouchDatabase(url=url)
    if not couch.load():
        print("ERROR: could not connect to CouchDB")
        return

    # Connect to S3.
    s3 = S3Database()
    if not s3.load():
        print("ERROR: could not connect to S3")
        return

    # Perform the migration.
    migrate_states(couch, s3)
    migrate_fpga_results(couch, s3)
    migrate_cacti_results(couch, s3)


if __name__ == '__main__':
    main()
