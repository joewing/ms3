
import os
import optparse
import boto.ec2

import database

parser = optparse.OptionParser()
parser.add_option('-i', '--iterations', dest='iterations', default=10,
                  help='total iterations requested')
parser.add_option('-s', '--start', dest='start', default=0,
                  help='max instances to start')

def start_instance(db, name):
   akey = os.environ['AWS_ACCESS_KEY']
   skey = os.environ['AWS_SECRET_KEY']
   image = os.environ['AWS_IMAGE']
   sgroup = os.environ['AWS_SECURITY_GROUP']
   region = os.eniron['AWS_REGION']
   conn = boto.ec2.connect_to_region(region,
                                     aws_access_key_id = akey,
                                     aws_secret_access_key = skey)
   r = conn.run_instances(image_id = image,
                          security_groups = [sgroup],
                          instance_type = 't1.micro',
                          instance_initiated_shutdown_behavior = 'terminate')
   for i in r.instances:
      print("  started " + str(i.id))
                      

def check(db, name, iterations, can_start):
   db.load(name)
   instance = db.get_instance()
   current = db.get_value('evaluations')
   print(name.name + ": " + str(current) + " / " + str(iterations))
   if instance != None:
      print("  running: " + instance)
      return False
   elif current >= iterations:
      print("  done")
      return False
   elif can_start:
      print("  starting instance...")
      start_instance(db, name)
      return True
   else:
      print("  pending")
      return False

def main():
   (options, args) = parser.parse_args()
   iterations = int(options.iterations)
   to_start = int(options.start)
   db = database.Database()
   started = 0
   for n in db.list():
      if check(db, n, iterations, started < to_start):
         started += 1
      print

if __name__ == '__main__':
   main()

