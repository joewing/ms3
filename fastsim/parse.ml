open Lex
open Base_memory
open Main_memory
open Machine
open Model

let parse_string token_list = fst @@ match_string token_list;;

let parse_int token_list = fst @@ match_int token_list;;

let parse_float token_list = float_of_string @@ parse_string token_list;;

let parse_bool token_list = fst @@ match_bool token_list;;

let rec match_argument token_list =
    let tok = peek_token token_list in
    let pos = get_position tok in
    match tok with
    | Literal _ ->
        let (value, token_list) = match_string token_list in
        let (following, token_list) = match_argument token_list in
        let result = (Literal (value, pos)) :: following in
        (result, token_list)
    | Open _ ->
        let token_list = match_open token_list in
        let (inside, token_list) = match_argument token_list in
        let token_list = match_close token_list in
        let (following, token_list) = match_argument token_list in
        let result = [Open pos] @ inside @ [Close pos] @ following in
        (result, token_list)
    | Close _ -> ([], token_list)
    | t -> parse_error [t] "invalid argument"
;;

let rec match_arguments setter token_list =
    match peek_token token_list with
    | Open _ ->
        begin
            let token_list = match_open token_list in
            let (name, token_list) = match_string token_list in
            let (value, token_list) = match_argument token_list in
            let token_list = match_close token_list in
            setter name value;
            match_arguments setter token_list
        end
    | _ -> token_list
;;

let match_label token_list =
    let token_list = match_open token_list in
    match match_string token_list with
    | ("label", token_list) ->
        let (label, token_list) = match_string token_list in
        let token_list = match_close token_list in
        token_list
    | _ -> parse_error token_list "expected 'label'"
;;

let set_mach mach name = function
    | [Literal (s, _)] -> set_machine mach name s
    | t -> parse_error t ("invalid machine argument: " ^ name)
;;

let match_machine token_list =
    let token_list = match_open token_list in
    let mach = create_machine () in
    match match_string token_list with
    | ("machine", token_list) ->
            let setter = set_mach mach in
            let token_list = match_arguments setter token_list in
            let token_list = match_close token_list in
            (mach, token_list)
    | _ -> parse_error token_list "expected 'machine'"
;;

let rec set_none name value = ()

and set_container mem name = function
    | [Literal (s, _)] -> mem#set name s
    | v when name = "memory" -> mem#set_next @@ parse_memory v
    | t -> parse_error t ("invalid argument: " ^ name)

and set_main mem name = function
    | [Literal (s, _)] -> mem#set name s
    | t -> parse_error t ("invalid argument" ^ name)

and set_opt mem name = function
    | [Literal (s, _)] -> mem#set name s
    | v when (String.length name) > 6 && (String.sub name 0 6) = "memory" ->
        let name_length = String.length name in
        let index = int_of_string @@ String.sub name 6 name_length in
        mem#set_memory index @@ parse_memory v
    | t -> parse_error t ("invalid argument" ^ name)

and set_transform mem name = function
    | [Literal (s, _)] -> mem#set name s
    | v when name = "memory" -> mem#set_next @@ parse_memory v
    | v when name = "bank" -> mem#set_bank @@ parse_memory v
    | t -> parse_error t ("invalid transform argument: " ^ name)

and set_split mem name = function
    | [Literal (s, _)] -> mem#set name s
    | v when name = "memory" -> mem#set_next @@ parse_memory v
    | v when name = "bank0" -> mem#set_bank0 @@ parse_memory v
    | v when name = "bank1" -> mem#set_bank1 @@ parse_memory v
    | t -> parse_error t ("invalid split argument: " ^ name)

and create_memory name =
    let wrap m = Some (m :> base_memory) in
    match name with
    | "main" -> (None, set_none)
    | "spm" -> let m = new Spm.spm in (wrap m, set_container m)
    | "dram" -> let m = new Dram.dram in (wrap m, set_main (m :> main_memory))
    | "pcm" -> let m = new Pcm.pcm in (wrap m, set_main (m :> main_memory))
    | "cache" -> let m = new Cache.cache in (wrap m, set_container m)
    | "offset" -> let m = new Offset.offset in (wrap m, set_transform m)
    | "xor" -> let m = new Xor.xor in (wrap m, set_transform m)
    | "shift" -> let m = new Shift.shift in (wrap m, set_transform m)
    | "split" -> let m = new Split.split in (wrap m, set_split m)
    | "join" -> let m = new join in (wrap m, set_none)
    | "prefetch" -> let m = new Prefetch.prefetch in (wrap m, set_container m)
    | "ram" -> let m = new Ram.ram in (wrap m, set_main (m :> main_memory))
    | "option" -> let m = new Opt.opt in (wrap m, set_main (m :> main_memory))
    | name -> parse_error [] ("invalid memory: " ^ name)

and match_memory token_list =
    let token_list = match_open token_list in
    let (name, token_list) = match_string token_list in
    let (mem, setter) = create_memory name in
    match mem with
    | None -> (None, match_close token_list)
    | Some m ->
            let token_list = match_arguments setter token_list in
            (mem, match_close token_list)

and parse_memory token_list = fst @@ match_memory token_list
;;

let set_subsystem mem name = function
    | [Literal (s, _)] -> mem#set name s
    | v when name = "memory" -> mem#set_next @@ parse_memory v
    | t -> parse_error t ("invalid argument: " ^ name)
;;

let set_fifo mem = set_subsystem (mem :> Subsystem.subsystem);;

let match_subsystem token_list =
    let token_list = match_open token_list in
    let token_list = match_literal "subsystem" token_list in
    let m = new Subsystem.subsystem in
    let token_list = match_arguments (set_subsystem m) token_list in
    let token_list = match_close token_list in
    (m, token_list)
;;

let match_fifo sub token_list =
    let token_list = match_open token_list in
    let token_list = match_literal "fifo" token_list in
    let m = if sub >= 0 then
            new Dummy_fifo.dummy_fifo
        else
            new Fifo.fifo
    in
    let token_list = match_arguments (set_fifo m) token_list in
    let token_list = match_close token_list in
    (m, token_list)
;;

let match_main_memory token_list =
    let token_list = match_open token_list in
    let token_list = match_literal "main" token_list in
    let token_list = match_open token_list in
    let token_list = match_literal "memory" token_list in
    let (main, token_list) = match_memory token_list in
    let token_list = match_close token_list in
    let token_list = match_close token_list in
    (main, token_list)
;;

let match_memory_list sub token_list =
    let token_list = match_open token_list in
    let token_list = match_literal "memory" token_list in
    let (main, token_list) = match_main_memory token_list in
    let rec helper token_list =
        match token_list with
        | Open _ :: Literal ("subsystem", _) :: _ ->
            let (mem, token_list) = match_subsystem token_list in
            let (subsystems, fifos, token_list) = helper token_list in
            (mem :: subsystems, fifos, token_list)
        | Open _ :: Literal ("fifo", _) :: _ ->
            let (mem, token_list) = match_fifo sub token_list in
            let (subsystems, fifos, token_list) = helper token_list in
            (subsystems, mem :: fifos, token_list)
        | _ -> ([], [], token_list)
    in
    let (subsystems, fifos, token_list) = helper token_list in
    let token_list = match_close token_list in
    match main with
    | Some main -> (main, subsystems, fifos, token_list)
    | None -> parse_error token_list "invalid main memory"
;;

let set_benchmark benchmark name = function
    | [Literal (value, _)] -> benchmark#set name value
    | t -> parse_error t "invalid benchmark argument"
;;

let match_benchmark = function
    | Literal ("trace", _) :: token_list ->
        let benchmark = new Trace_benchmark.trace_benchmark in
        let token_list = match_arguments (set_benchmark benchmark) token_list in
        (benchmark, token_list)
    | Literal ("split", _) :: token_list ->
        let benchmark = new Split_benchmark.split_benchmark in
        let token_list = match_arguments (set_benchmark benchmark) token_list in
        (benchmark, token_list)
    | t -> parse_error t "invalid benchmark"
;;

let rec match_benchmark_list sub = function
    | Open _ :: token_list ->
            let (benchmark, token_list) = match_benchmark token_list in
            let token_list = match_close token_list in
            let (next, token_list) = match_benchmark_list sub token_list in
            begin
                if sub >= 0 && benchmark#id != sub then
                    begin
                        benchmark#set "last" "false";
                        benchmark#set "ignore" "true"
                    end
                else if sub >= 0 then
                    begin
                        benchmark#set "last" "true";
                        benchmark#set "ignore" "false"
                    end
                else ();
                (benchmark :: next, token_list)
            end
    | token_list -> ([], token_list)
;;

let match_benchmarks token_list sub =
    let token_list = match_open token_list in
    let token_list = match_literal "benchmarks" token_list in
    let (result, token_list) = match_benchmark_list sub token_list in
    (result, match_close token_list)
;;

let parse_model token_list sub : Model.model =
    let token_list = match_label token_list in
    let (mach, token_list) = match_machine token_list in
    let (main, subsystems, fifos, token_list)
        = match_memory_list sub token_list in
    let (benchmarks, token_list) = match_benchmarks token_list sub in
    { mach; main; subsystems; fifos; benchmarks }
;;

let parse_model_file file_name subsystem_index =
    parse_model (tokenize file_name) subsystem_index
;;
