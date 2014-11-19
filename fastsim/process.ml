open Subsystem
open Benchmark

type process = {
    produce : process -> int -> int;
    consume : process -> int -> int;
    peek : process -> int -> int -> int;
    run : string -> access stream;
    directory : string;
    mem : subsystem;
    output : Compress.compress;
    trace_queues : bool;
    mutable accesses : access stream;
    mutable pending_access : access option;
}

exception End_simulation

type producer = process -> int -> int

type consumer = process -> int -> int

type peeker = process -> int -> int -> int

type runner = string -> access stream

let create_process produce consume peek run directory mem synthetic tq =
    {
        produce = produce;
        consume = consume;
        peek = peek;
        run = run;
        directory = directory;
        mem = mem;
        output = new Compress.compress;
        accesses = SNil;
        pending_access = None;
        trace_queues = tq && not synthetic;
    }
;;

let trace_produce proc addr t =
    if proc.trace_queues then
        proc.output#trace_produce addr t
    else ()
;;

let trace_consume proc addr t =
    if proc.trace_queues then
        proc.output#trace_consume addr t
    else ()
;;

let get_trace proc = proc.output#get_output;;

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
    let start = proc.mem#process 0 false addr size in
    proc.mem#process start true addr size
;;

let process_input proc addr size =
    let result = proc.consume proc addr in
    let result = if result < 0 then proc.consume proc size else result in
    proc.pending_access <- None;
    max 0 result
;;

let process_output proc addr size =
    let result = proc.produce proc addr in
    let result = if result < 0 then proc.produce proc size else result in
    proc.pending_access <- None;
    max 0 result
;;

let process_access proc t addr size =
    match t with
    | 'R' -> proc.mem#process 0 false addr size
    | 'W' -> proc.mem#process 0 true addr size
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
    | SNil -> raise End_simulation
;;

let process_step proc =
    match proc.pending_access with
    | Some (t, addr, size)  -> process_access proc t addr size
    | None                  -> process_next proc
;;

let process_is_done proc =
    match proc.accesses with
    | SCons _ -> false
    | SNil -> true
;;
