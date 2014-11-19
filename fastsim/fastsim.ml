open Arg
open Parse
open Simulator

(* Options. *)
let directory = ref "/data/traces/";;
let experiment = ref "";;
let subsystem_index = ref (-1);;
let channel_index = ref (-1);;
let trace_queues = ref false;;

(* Functions for parsing the command line. *)
let set_directory d = directory := d;;
let set_experiment e = experiment := e;;
let set_subsystem_index s = subsystem_index := s;;
let set_channel_index i = channel_index := i;;
let set_trace_queues () = trace_queues := true;;
let opts = [
    ("-d", Arg.String set_directory, "trace directory");
    ("-s", Arg.Int set_subsystem_index, "subsystem to evaluate");
    ("-c", Arg.Int set_channel_index, "trace channel interaction");
    ("-q", Arg.Unit set_trace_queues, "trace queue interaction")
];;

let run_experiment () =
    let m = parse_model_file !experiment !subsystem_index !channel_index in
    let sim = create_simulator !directory m !trace_queues in
    run_simulator sim
;;

let main () =
    Arg.parse opts set_experiment "usage: sim [options] <experiment>";
    try
        run_experiment ()
    with Lex.ParseError msg ->
        print_endline @@ "ERROR: " ^ msg
;;

main ()
