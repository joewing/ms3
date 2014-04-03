open Base_memory
open Trace
open Pq

class process simulator benchmark directory (mem : base_memory) =
    object (self)

        val mutable accesses = SNil
        val mutable consume_waiting : int = -1
        val mutable produce_waiting : int = -1
        val mutable peek_waiting : int * int = -1, -1

        method finish = mem#finish

        method reset m main =
            mem#reset m main;
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

class simulator mach directory main mem_list benchmarks =
    object (self)

        val mutable processes : process list = []
        val consumers = Hashtbl.create 64
        val producers = Hashtbl.create 64
        val heap : process pq = new pq

        method private get_subsystem (index : int) : base_memory =
            List.find (fun m ->
                not m#is_fifo && m#id = index
            ) mem_list

        method private get_fifo (index : int) : base_memory =
            List.find (fun m -> m#is_fifo && m#id = index) mem_list

        method private add_benchmark (b : Trace.trace) =
            let mem = self#get_subsystem b#id in
            let proc = new process self b directory mem in
            processes <- proc :: processes

        method private all_fifos : base_memory list =
            List.filter (fun m -> m#is_fifo) mem_list

        method private all_subsystems : base_memory list =
            List.filter (fun m -> not m#is_fifo) mem_list

        method private all_memories : base_memory list = mem_list

        method private align word_size addr =
            let temp = addr land (word_size - 1) in
            if temp = 0 then addr
            else addr + (word_size - temp) 

        method produce (proc : process) (index : int) =
            let fifo = self#get_fifo index in
            let rc = fifo#produce () in
            begin
                if rc < 0 then
                    Hashtbl.add producers index proc
                else 
                    try
                        let c = Hashtbl.find consumers index in
                        heap#push mach#time c;
                        Hashtbl.remove consumers index
                    with Not_found -> ()
            end; rc

        method consume (proc : process) (index : int) =
            let fifo = self#get_fifo index in
            let rc = fifo#consume () in
            begin
                if rc < 0 then
                    Hashtbl.add consumers index proc
                else
                    try
                        let p = Hashtbl.find producers index in
                        heap#push mach#time p;
                        Hashtbl.remove producers index
                    with Not_found -> ()
            end; rc

        method peek (proc : process) (index : int) (offset : int) =
            let fifo = self#get_fifo index in
            let rc = fifo#peek offset in
            begin
                if rc < 0 then
                    Hashtbl.add consumers index proc
                else
                    try
                        let p = Hashtbl.find producers index in
                        heap#push mach#time p;
                        Hashtbl.remove producers index
                    with Not_found -> ()
            end; rc

        method private reset () =
            mach#reset ();
            processes <- [];
            Hashtbl.clear producers;
            Hashtbl.clear consumers;
            List.iter self#add_benchmark benchmarks;
            let offset = ref 0 in
            List.iter (fun f ->
                offset := self#align f#word_size !offset;
                f#set_offset !offset;
                f#reset mach main;
                offset := !offset + f#total_size
            ) self#all_fifos;
            List.iter (fun m ->
                offset := self#align m#word_size !offset;
                m#set_offset !offset;
                offset := !offset + m#total_size
            ) self#all_subsystems;
            List.iter (fun p ->
                p#reset mach main;
                heap#push 0 p
            ) processes

        method private scores =
            List.map (fun m ->
                let prefix = if m#is_fifo then "fifo" else "subsystem" in
                let name = prefix ^ (string_of_int m#id) in
                (name, m#score)
            ) self#all_memories

        method run =
            self#reset ();
            while not heap#empty do
                mach#set_time @@ max mach#time heap#key;
                let proc = heap#pop in
                let delta = proc#step in
                if delta >= 0 then
                    let next_time = mach#time + delta in
                    heap#push next_time proc
            done;
            List.iter (fun p ->
                let t = p#finish in
                mach#set_time @@ max mach#time t
            ) processes;
            ("total", mach#time) :: self#scores

    end
