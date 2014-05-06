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

type simulator = {
    model : model;
    directory : string;
    mutable processes : process list;
    consumers : (int, process) Hashtbl.t;
    producers : (int, process) Hashtbl.t;
    heap : process Pq.t;
    subsystem_map : subsystem IntMap.t;
    fifo_map : fifo IntMap.t;
}

let create_simulator directory model =
    let subsystem_count = List.length model.subsystems in
    let fifo_count = List.length model.fifos in
    let subsystem_map = List.fold_left (fun acc s ->
        IntMap.add s#id s acc
    ) IntMap.empty model.subsystems in
    let fifo_map = List.fold_left (fun acc f ->
        IntMap.add f#id f acc
    ) IntMap.empty model.fifos in
    {
        model = model;
        directory = directory;
        processes = [];
        consumers = Hashtbl.create fifo_count;
        producers = Hashtbl.create fifo_count;
        heap = Pq.create (subsystem_count + fifo_count);
        subsystem_map = subsystem_map;
        fifo_map = fifo_map;
    }
;;

let get_subsystem sim index =
    try
        IntMap.find index sim.subsystem_map
    with Not_found ->
        failwith @@ "Subsystem " ^ (string_of_int index) ^ " not found"
;;

let get_fifo sim index =
    try
        IntMap.find index sim.fifo_map
    with Not_found ->
        failwith @@ "FIFO " ^ (string_of_int index) ^ " not found"
;;

let all_memories sim =
    let m = sim.model in
    m.subsystems @ (m.fifos :> subsystem list)
;;

let align (word_size : int) (addr : int) =
    let temp = addr land (word_size - 1) in
    if temp = 0 then addr else addr + (word_size - temp)
;;

let produce sim proc (index : int) =
    let fifo = get_fifo sim index in
    let rc = fifo#produce in
    begin
        if rc < 0 then
            Hashtbl.add sim.producers index proc
        else
            try
                let c = Hashtbl.find sim.consumers index in
                Pq.push sim.heap sim.model.mach.time c;
                Hashtbl.remove sim.consumers index
            with Not_found -> ()
    end; rc
;;


let consume sim proc (index : int) =
    let fifo = get_fifo sim index in
    let rc = fifo#consume in
    begin
        if rc < 0 then
            Hashtbl.add sim.consumers index proc
        else
            try
                let p = Hashtbl.find sim.producers index in
                Pq.push sim.heap sim.model.mach.time p;
                Hashtbl.remove sim.producers index
            with Not_found -> ()
    end; rc
;;

let peek sim proc (index : int) (offset : int) =
    let fifo = get_fifo sim index in
    let rc = fifo#peek offset in
    begin
        if rc < 0 then
            Hashtbl.add sim.consumers index proc
        else
            try
                let p = Hashtbl.find sim.producers index in
                Pq.push sim.heap sim.model.mach.time p;
                Hashtbl.remove sim.producers index
            with Not_found -> ()
    end; rc
;;

let add_benchmark sim b =
    let mem = (get_subsystem sim b#id :> base_memory) in
    let produce = produce sim in
    let consume = consume sim in
    let peek = peek sim in
    let run = b#run in
    let proc = create_process produce consume peek run sim.directory mem in
    sim.processes <- proc :: sim.processes
;;

let reset_simulator sim =
    reset_machine sim.model.mach;
    sim.processes <- [];
    Hashtbl.clear sim.producers;
    Hashtbl.clear sim.consumers;
    List.iter (add_benchmark sim) sim.model.benchmarks;
    let offset = ref 0 in
    List.iter (fun m ->
        offset := align m#word_size !offset;
        m#set_offset !offset;
        m#reset sim.model.mach sim.model.main;
        offset := !offset + m#total_size
    ) (all_memories sim);
    List.iter (fun p ->
        process_reset p;
        Pq.push sim.heap 0 p
    ) sim.processes
;;

let get_scores sim =
    let subsystem_scores = List.map (fun m ->
        let name = "subsystem" ^ (string_of_int m#id) in
        (name, m#score)
    ) sim.model.subsystems in
    let fifo_scores = List.map (fun m ->
        let name = "fifo" ^ (string_of_int m#id) in
        (name, m#score)
    ) sim.model.fifos in
    let totals = [
        ("total", sim.model.mach.time);
        ("writes", sim.model.main#writes)
    ] in subsystem_scores @ fifo_scores @ totals
;;

let check_done sim =
    let has_done = List.exists process_is_done sim.processes in
    if not has_done then
        failwith "invalid trace"
;;

let run_simulator sim =
    reset_simulator sim;
    while not (Pq.is_empty sim.heap) do
        sim.model.mach.time <- max sim.model.mach.time (Pq.get_key sim.heap);
        let proc = Pq.pop sim.heap in
        let delta = process_step proc in
        if delta >= 0 then
            let next_time = sim.model.mach.time + delta in
            Pq.push sim.heap next_time proc
    done;
    check_done sim;
    List.iter (fun p ->
        let t = process_finish p in
        sim.model.mach.time <- max sim.model.mach.time t
    ) sim.processes;
    get_scores sim
;;
