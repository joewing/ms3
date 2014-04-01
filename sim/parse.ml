open Lex
open Base_memory

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

let set_machine mach name = function
    | [Literal (s, _)] -> mach#set name s
    | t -> parse_error t ("invalid machine argument: " ^ name)
;;

let match_machine token_list =
    let token_list = match_open token_list in
    let mach = new Machine.machine in
    match match_string token_list with
    | ("machine", token_list) ->
            let setter = set_machine mach in
            let token_list = match_arguments setter token_list in
            let token_list = match_close token_list in
            (mach, token_list)
    | _ -> parse_error token_list "expected 'machine'"
;;

let rec set_none name value = ()

and set_subsystem mem name = function
    | [Literal (s, _)] -> mem#set name s
    | v when name = "memory" -> mem#set_next @@ parse_memory v
    | t -> parse_error t ("invalid subsystem argument: " ^ name)

and set_fifo mem name = function
    | [Literal (s, _)] -> mem#set name s
    | v when name = "memory" -> mem#set_next @@ parse_memory v
    | t -> parse_error t ("invalid fifo argument: " ^ name)

and set_spm mem name = function
    | [Literal (s, _)] -> mem#set name s
    | v when name = "memory" -> mem#set_next @@ parse_memory v
    | t -> parse_error t ("invalid spm argument: " ^ name)

and set_dram mem name = function
    | [Literal (s, _)] -> mem#set name s
    | t -> parse_error t ("invalid dram argument" ^ name)

and set_cache mem name = function
    | [Literal (s, _)] -> mem#set name s
    | v when name = "memory" -> mem#set_next @@ parse_memory v
    | t -> parse_error t ("invalid cache argument: " ^ name)

and set_transform mem name = function
    | [Literal (s, _)] -> mem#set name s
    | v when name = "memory" -> mem#set_next @@ parse_memory v
    | v when name = "bank" -> mem#set_bank @@ parse_memory v
    | t -> parse_error t ("invalid transform argument: " ^ name)

and create_memory name =
    let wrap m = Some (m :> base_memory) in
    match name with
    | "main" -> (None, set_none)
    | "subsystem" ->
        let m = new Subsystem.subsystem in (wrap m, set_subsystem m)
    | "fifo" -> let m = new Fifo.fifo in (wrap m, set_fifo m)
    | "spm" -> let m = new Spm.spm in (wrap m, set_spm m)
    | "dram" -> let m = new Dram.dram in (wrap m, set_dram m)
    | "cache" -> let m = new Cache.cache in (wrap m, set_cache m)
    | "offset" -> let m = new Offset.offset in (wrap m, set_transform m)
    | "xor" -> let m = new Xor.xor in (wrap m, set_transform m)
    | "shift" -> let m = new Shift.shift in (wrap m, set_transform m)
    | "join" -> let m = new join in (wrap m, set_none)
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

let match_memory_list token_list =
    let token_list = match_open token_list in
    let token_list = match_literal "memory" token_list in
    let (main, token_list) = match_main_memory token_list in
    let rec helper token_list =
        match peek_token token_list with
        | Open _ ->
                begin
                    let (mem, token_list) = match_memory token_list in
                    let (next, token_list) = helper token_list in
                    match mem with
                    | Some mem -> (mem :: next, token_list)
                    | None     -> (next, token_list)
                end
        | _ -> ([], token_list)
    in
    let (lst, token_list) = helper token_list in
    match main with
    | Some main -> (main, lst, match_close token_list)
    | None -> parse_error token_list "invalid main memory"
;;

let rec match_benchmark_list = function
    | Open _ :: token_list ->
            let token_list = match_literal "trace" token_list in
            let benchmark = new Trace.trace in
            let set_benchmark name = function
                | [Literal (value, _)] -> benchmark#set name value
                | t -> parse_error t "invalid trace argument"
            in
            let token_list = match_arguments set_benchmark token_list in
            let token_list = match_close token_list in
            let (next, token_list) = match_benchmark_list token_list in
            (benchmark :: next, token_list)
    | token_list -> ([], token_list)
;;

let match_benchmarks token_list =
    let token_list = match_open token_list in
    let token_list = match_literal "benchmarks" token_list in
    let (result, token_list) = match_benchmark_list token_list in
    (result, match_close token_list)
;;

let parse_model token_list =
    let (mach, token_list) = match_machine token_list in
    let (main, mem_list, token_list) = match_memory_list token_list in
    let (benchmarks, token_list) = match_benchmarks token_list in
    (mach, main, mem_list, benchmarks)
;;

let parse_model_file file_name = parse_model @@ tokenize file_name;;
