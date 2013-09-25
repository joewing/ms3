
from Memory import *
from Memory.SPM import *
from Memory.RAM import *
from Memory.Offset import *
from Process import *
from Optimizer import *
from Machine import MachineType
from Benchmark.HashBenchmark import HashBenchmark

def main():
   machine = MachineType()
   ram = RAM(machine)
   mem1 = SPM(machine, mem = ram, word_count = 4)
   mem2 = Offset(machine, mem = mem1, offset = 32)
#   proc1 = Process(mem1, HashBenchmark(machine, 5, 10))
   proc2 = Process(mem2, HashBenchmark(machine, 6, 10))
   pl = ProcessList(machine)
#   pl.insert(proc1)
   pl.insert(proc2)
   print str(proc2.mem)
   o = Optimizer(pl)
   o.modify()
   print str(proc2.mem)
   pl.run()
   print("Time: " + str(machine.time))

main()
