open Base_memory
open Subsystem
open Fifo
open Model
open Process
open Machine

module Int = struct
    type t = int
    let compare = compare
end

module IntMap = Map.Make(Int)

class simulator directory model =
    object (self)

        val mutable processes : process list = []
        val consumers = Hashtbl.create (List.length model.fifos)
        val producers = Hashtbl.create (List.length model.fifos)

        val heap : process Pq.t =
            let subsystem_count = List.length model.subsystems in
            let fifo_count = List.length model.fifos in
            Pq.create (subsystem_count + fifo_count)

        val subsystem_map : subsystem IntMap.t =
            List.fold_left (fun acc s ->
                IntMap.add s#id s acc
            ) IntMap.empty model.subsystems

        val fifo_map : fifo IntMap.t =
            List.fold_left (fun acc f ->
                IntMap.add f#id f acc
            ) IntMap.empty model.fifos

        method private get_subsystem (index : int) : subsystem =
            try
                IntMap.find index subsystem_map
            with Not_found ->
                failwith @@ "Subsystem " ^ (string_of_int index) ^ " not found"

        method private get_fifo (index : int) : fifo =
            try
                IntMap.find index fifo_map
            with Not_found ->
                failwith @@ "FIFO " ^ (string_of_int index) ^ " not found"

        method private add_benchmark (b : Trace.trace) =
            let mem = (self#get_subsystem b#id :> base_memory) in
            let proc = create_process self b directory mem in
            processes <- proc :: processes

        method private all_memories : subsystem list =
            model.subsystems @ (model.fifos :> subsystem list)

        method private align word_size addr =
            let temp = addr land (word_size - 1) in
            if temp = 0 then addr
            else addr + (word_size - temp) 

        method produce (proc : process) (index : int) =
            let fifo = self#get_fifo index in
            let rc = fifo#produce in
            begin
                if rc < 0 then
                    Hashtbl.add producers index proc
                else 
                    try
                        let c = Hashtbl.find consumers index in
                        Pq.push heap model.mach.time c;
                        Hashtbl.remove consumers index
                    with Not_found -> ()
            end; rc

        method consume (proc : process) (index : int) =
            let fifo = self#get_fifo index in
            let rc = fifo#consume in
            begin
                if rc < 0 then
                    Hashtbl.add consumers index proc
                else
                    try
                        let p = Hashtbl.find producers index in
                        Pq.push heap model.mach.time p;
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
                        Pq.push heap model.mach.time p;
                        Hashtbl.remove producers index
                    with Not_found -> ()
            end; rc

        method private reset () =
            reset_machine model.mach;
            processes <- [];
            Hashtbl.clear producers;
            Hashtbl.clear consumers;
            List.iter self#add_benchmark model.benchmarks;
            let offset = ref 0 in
            List.iter (fun m ->
                offset := self#align m#word_size !offset;
                m#set_offset !offset;
                m#reset model.mach model.main;
                offset := !offset + m#total_size
            ) self#all_memories;
            List.iter (fun p ->
                process_reset p;
                Pq.push heap 0 p
            ) processes

        method private scores =
            let subsystem_scores = List.map (fun m ->
                let name = "subsystem" ^ (string_of_int m#id) in
                (name, m#score)
            ) model.subsystems in
            let fifo_scores = List.map (fun m ->
                let name = "fifo" ^ (string_of_int m#id) in
                (name, m#score)
            ) model.fifos in
            subsystem_scores @ fifo_scores

        method run =
            self#reset ();
            while not (Pq.is_empty heap) do
                model.mach.time <- max model.mach.time (Pq.get_key heap);
                let proc = Pq.pop heap in
                let delta = process_step proc in
                if delta >= 0 then
                    let next_time = model.mach.time + delta in
                    Pq.push heap next_time proc
            done;
            List.iter (fun p ->
                let t = process_finish p in
                model.mach.time <- max model.mach.time t
            ) processes;
            ("total", model.mach.time) :: self#scores

    end
