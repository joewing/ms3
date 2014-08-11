open Arg
open Parse
open Simulator

(* Options. *)
let directory = ref "/data/traces/";;
let experiment = ref "";;
let subsystem_index = ref (-1);;

(* Functions for parsing the command line. *)
let set_directory d = directory := d;;
let set_experiment e = experiment := e;;
let set_subsystem_index s = subsystem_index := s;;
let opts = [
    ("-d", Arg.String set_directory, "trace directory");
    ("-s", Arg.Int set_subsystem_index, "subsystem to evaluate")
];;

let run_experiment () =
    let m = parse_model_file !experiment !subsystem_index in
    let sim = create_simulator !directory m in
    let results = run_simulator sim in
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
