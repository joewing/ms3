type model = {
    mach : Machine.machine;
    main : Main_memory.main_memory;
    subsystems : Subsystem.subsystem list;
    fifos : Fifo.fifo list;
    benchmarks : Trace_benchmark.trace_benchmark list;
}
