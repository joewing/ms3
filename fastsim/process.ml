open Base_memory
open Trace

class process simulator benchmark directory (mem : base_memory) =
    object (self)

        val mutable accesses = SNil
        val mutable consume_waiting : int = -1
        val mutable produce_waiting : int = -1
        val mutable peek_waiting : int * int = -1, -1

        method finish = mem#finish

        method reset () =
            accesses <- benchmark#run directory

        method private process_pending_consume =
            let temp = simulator#consume self consume_waiting in
            consume_waiting <- if temp >= 0 then -1 else consume_waiting;
            temp

        method private process_pending_produce =
            let temp = simulator#produce self produce_waiting in
            produce_waiting <- if temp >= 0 then -1 else produce_waiting;
            temp

        method private process_pending_peek =
            let addr, size = peek_waiting in
            let temp = simulator#peek self addr size in
            peek_waiting <- if temp >= 0 then (-1, -1) else peek_waiting;
            temp

        method private process_produce addr =
            let temp = simulator#produce self addr in
            produce_waiting <- if temp < 0 then addr else -1;
            temp

        method private process_consume addr =
            let temp = simulator#consume self addr in
            consume_waiting <- if temp < 0 then addr else -1;
            temp

        method private process_peek addr size =
            let temp = simulator#peek self addr size in
            peek_waiting <- if temp < 0 then (addr, size) else (-1, -1);
            temp

        method private process_modify addr size =
            let start = send_request mem 0 false addr size in
            send_request mem start true addr size

        method private process_access t addr size =
            match t with
            | 'R' -> send_request mem 0 false addr size
            | 'W' -> send_request mem 0 true addr size
            | 'M' -> self#process_modify addr size
            | 'I' -> addr
            | 'P' -> self#process_produce addr
            | 'C' -> self#process_consume addr
            | 'K' -> self#process_peek addr size
            | 'X' -> -1
            | _ -> failwith @@ "invalid access: " ^ (String.make 1 t)

        method step =
            if consume_waiting >= 0 then
                self#process_pending_consume
            else if produce_waiting >= 0 then
                self#process_pending_produce
            else if (fst peek_waiting) >= 0 then
                self#process_pending_peek
            else
                match accesses with
                | SCons ((t, addr, size), next) ->
                        accesses <- next ();
                        self#process_access t addr size
                | SNil -> -1

    end
