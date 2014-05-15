open Base_memory
open Benchmark
open Trace

type process = {
    produce : process -> int -> int;
    consume : process -> int -> int;
    peek : process -> int -> int -> int;
    run : string -> access stream;
    directory : string;
    mem : base_memory;
    mutable accesses : access stream;
    mutable pending_access : access option;
}

type producer = process -> int -> int

type consumer = process -> int -> int

type peeker = process -> int -> int -> int

type runner = string -> access stream

let create_process produce consume peek run directory mem =
    {
        produce = produce;
        consume = consume;
        peek = peek;
        run = run;
        directory = directory;
        mem = mem;
        accesses = SNil;
        pending_access = None;
    }
;;

let process_produce proc addr =
    let result = proc.produce proc addr in
    proc.pending_access <-
        if result < 0 then Some ('P', addr, 0)
        else None;
    result
;;

let process_consume proc addr =
    let result = proc.consume proc addr in
    proc.pending_access <-
        if result < 0 then Some ('C', addr, 0)
        else None;
    result
;;

let process_peek proc addr size =
    let result = proc.peek proc addr size in
    proc.pending_access <-
        if result < 0 then Some ('K', addr, size)
        else None;
    result
;;

let process_modify proc addr size =
    let start = proc.mem#send_request 0 false addr size in
    proc.mem#send_request start true addr size
;;

let process_input proc addr size =
    let result = proc.consume proc addr in
    let result = if result < 0 then proc.consume proc size else result in
    proc.pending_access <-
        if result < 0 then Some ('A', addr, size)
        else None;
    result
;;

let process_output proc addr size =
    let result = proc.produce proc addr in
    let result = if result < 0 then proc.produce proc size else result in
    proc.pending_access <-
        if result < 0 then Some ('O', addr, size)
        else None;
    result
;;

let process_access proc t addr size =
    match t with
    | 'R' -> proc.mem#send_request 0 false addr size
    | 'W' -> proc.mem#send_request 0 true addr size
    | 'M' -> process_modify proc addr size
    | 'I' -> addr
    | 'P' -> process_produce proc addr
    | 'C' -> process_consume proc addr
    | 'K' -> process_peek proc addr size
    | 'A' -> process_input proc addr size
    | 'O' -> process_output proc addr size
    | 'X' -> -1
    | _ -> failwith @@ "invalid access: " ^ (String.make 1 t)
;;

let process_reset proc = proc.accesses <- proc.run proc.directory;;

let process_next proc =
    match proc.accesses with
    | SCons ((t, addr, size), next) ->
            proc.accesses <- next ();
            process_access proc t addr size
    | SNil -> -1
;;

let process_step proc =
    match proc.pending_access with
    | Some (t, addr, size)  -> process_access proc t addr size
    | None                  -> process_next proc
;;

let process_finish proc = proc.mem#finish;;

let process_is_done proc =
    match proc.accesses with
    | SCons _ -> false
    | SNil -> true
;;
