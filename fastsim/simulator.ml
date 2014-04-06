open Base_memory
open Model
open Process
open Trace
open Pq

class simulator directory model =
    object (self)

        val mach : Machine.machine = model.mach
        val main : Main_memory.main_memory = model.main
        val subsystems : Subsystem.subsystem list = model.subsystems
        val fifos : Fifo.fifo list = model.fifos
        val benchmarks : Trace.trace list = model.benchmarks
        val mutable processes : process list = []
        val consumers = Hashtbl.create 64
        val producers = Hashtbl.create 64
        val heap : process pq = new pq

        method private get_subsystem (index : int) : Subsystem.subsystem =
            List.find (fun m -> m#id = index) subsystems

        method private get_fifo (index : int) : Fifo.fifo =
            List.find (fun m -> m#id = index) fifos

        method private add_benchmark (b : Trace.trace) =
            let mem = (self#get_subsystem b#id :> base_memory) in
            let proc = new process self b directory mem in
            processes <- proc :: processes

        method private all_memories : Subsystem.subsystem list =
            subsystems @ (fifos :> Subsystem.subsystem list)

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
            List.iter (fun m ->
                offset := self#align m#word_size !offset;
                m#set_offset !offset;
                m#reset mach main;
                offset := !offset + m#total_size
            ) self#all_memories;
            List.iter (fun p ->
                p#reset ();
                heap#push 0 p
            ) processes

        method private scores =
            let subsystem_scores = List.map (fun m ->
                let name = "subsystem" ^ (string_of_int m#id) in
                (name, m#score)
            ) subsystems in
            let fifo_scores = List.map (fun m ->
                let name = "fifo" ^ (string_of_int m#id) in
                (name, m#score)
            ) fifos in
            subsystem_scores @ fifo_scores

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
