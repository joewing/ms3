open Parse
open Simulator

let main () =
    let directory = "/home/mercury/jgw1/code/mblast/systems/Brutus/BLASTN/sim/old-trace/" in
    let (mach, main, mem, benchmarks) = parse_model_file "t" in
    let sim = new simulator mach directory main mem benchmarks in
    let t = sim#run in
    print_int t;
    print_newline ()
;;
main ()
