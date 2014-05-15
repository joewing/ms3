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
    mutable consume_waiting : int;
    mutable produce_waiting : int;
    mutable peek_waiting : int * int
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
        consume_waiting = -1;
        produce_waiting = -1;
        peek_waiting = (-1, -1);
    }
;;

let process_produce proc addr =
    let result = proc.produce proc addr in
    proc.produce_waiting <- if result < 0 then addr else -1;
    result
;;

let process_consume proc addr =
    let result = proc.consume proc addr in
    proc.consume_waiting <- if result < 0 then addr else -1;
    result
;;

let process_peek proc addr size =
    let result = proc.peek proc addr size in
    proc.peek_waiting <- if result < 0 then (addr, size) else (-1, -1);
    result
;;

let process_modify proc addr size =
    let start = proc.mem#send_request 0 false addr size in
    proc.mem#send_request start true addr size
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
    | 'X' -> -1
    | _ -> failwith @@ "invalid access: " ^ (String.make 1 t)
;;

let process_reset proc = proc.accesses <- proc.run proc.directory;;

let process_step proc =
    if proc.consume_waiting >= 0 then
        process_consume proc proc.consume_waiting
    else if proc.produce_waiting >= 0 then
        process_produce proc proc.produce_waiting
    else if (fst proc.peek_waiting) >= 0 then
        let addr, size = proc.peek_waiting in
        process_peek proc addr size
    else
        match proc.accesses with
        | SCons ((t, addr, size), next) ->
                proc.accesses <- next ();
                process_access proc t addr size
        | SNil -> -1
;;

let process_finish proc = proc.mem#finish;;

let process_is_done proc =
    match proc.accesses with
    | SCons _ -> false
    | SNil -> true
;;
