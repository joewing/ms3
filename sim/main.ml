open Parse
open Simulator

let main () =
    let directory = "/data/traces/" in
    let (mach, main, mem, benchmarks) = parse_model_file "t" in
    let sim = new simulator mach directory main mem benchmarks in
    let t = sim#run in
    print_int t;
    print_newline ()
;;
main ()
