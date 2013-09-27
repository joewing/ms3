
from Memory import *
from Memory.SPM import *
from Memory.RAM import *
from Memory.Offset import *
from Process import *
from Optimizer import *
from Machine import MachineType
from Benchmark.HashBenchmark import HashBenchmark
from Benchmark.MMBenchmark import MMBenchmark
from Distribution import Distribution

def main():
   machine = MachineType()
   ram = RAM(machine)
   mem1 = SPM(machine, mem = ram, size = 64)
   join = Join(machine)
   mem2 = Offset(machine, bank = join, mem = mem1, offset = 32)
   join.parent = mem2
   proc1 = Process(mem1, MMBenchmark(machine, 64, 1))
   proc2 = Process(mem2, HashBenchmark(machine, 6, 10000))
   pl = ProcessList(machine)
   pl.insert(proc1)
   pl.insert(proc2)

   rand = Distribution(15)
   o = Optimizer(machine, rand, pl)
   time = pl.run(rand)
   while o.optimize(time):
      time = pl.run()

if __name__ == '__main__':
   main()

