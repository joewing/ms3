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
    let (mach, main, subs, fifos, benchmarks) = parse_model_file !experiment in
    let sim = new simulator mach !directory main subs fifos benchmarks in
    let results = sim#run in
    List.iter (fun (name, value) ->
        Printf.printf "%s %d\n" name value
    ) results
;;

let main () =
    Arg.parse opts set_experiment "usage: sim [options] <experiment>";
    try
        run_experiment ()
    with Lex.ParseError msg ->
        print_endline @@ "ERROR: " ^ msg
;;

main ()
