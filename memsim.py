
import Memory.RAM
import Process
import Optimizer
import Machine
import Benchmark.Hash
import Benchmark.MM

def main():
   seed = 7
   machine = Machine.MachineType()
   ram = Memory.RAM.RAM(machine)
   proc1 = Process.Process(seed, ram, Benchmark.MM.MM(machine, 64, 1))
   proc2 = Process.Process(seed, ram, Benchmark.Hash.Hash(machine, 6, 10000))
   pl = Process.ProcessList(machine)
   pl.insert(proc1)
   pl.insert(proc2)

   o = Optimizer.Optimizer(machine, pl, seed = seed, iterations = 1000)
   time = pl.run(True)
   while o.optimize(time):
      time = pl.run()

if __name__ == '__main__':
   main()

