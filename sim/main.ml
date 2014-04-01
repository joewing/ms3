open Arg
open Parse
open Simulator

(* Options. *)
let directory = ref "/data/traces/";;
let experiment = ref "";;

(* Functions for parsing the command line. *)
let set_directory d = directory := d;;
let set_experiment e = experiment := e;;
let opts = [
    ("-d", Arg.String set_directory, "trace directory")
];;

let run_experiment () =
    let (mach, main, mem, benchmarks) = parse_model_file !experiment in
    let sim = new simulator mach !directory main mem benchmarks in
    let time = sim#run in
    print_int time; print_newline ()
;;

let main () =
    Arg.parse opts set_experiment "usage: sim [options] <experiment>";
    run_experiment ()
;;

main ()
