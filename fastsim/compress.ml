
let dict_size_bits = 16;;
let dict_size = 1 lsl dict_size_bits;;

module Int = struct
    type t = int
    let compare = compare
end

module IntSet = Set.Make(Int)

class compress =
    object (self)

        (* Sliding window dictionary. *)
        val dictionary = Array.make dict_size 0
        val mutable cursor = 0

        (* Mapping of values to positions in the dictionary. *)
        val value_map : (int, IntSet.t) Hashtbl.t = Hashtbl.create dict_size

        (* The current input buffer. *)
        val buffer = Array.make dict_size 0
        val mutable buffer_offset = 0
        val mutable buffer_starts = IntSet.empty

        (* The output.
         * Reprsentation is:
         *      [index:n][count:n], [value:32], ...
         *)
        val mutable output : int list = []

        (* Last time sent, used for produce/consume traces. *)
        val mutable last_time : int = 0

        method private start_run value =
            buffer.(0) <- value;
            buffer_offset <- 1;
            try
                buffer_starts <- Hashtbl.find value_map value
            with Not_found -> self#finish_run

        method private finish_run =
            if buffer_offset > 1 then (* Multiple values in this run. *)
                begin

                    (* Output the run and new value. *)
                    let index = IntSet.choose buffer_starts in
                    let last = buffer_offset - 1 in
                    let temp1 = (index lsl dict_size_bits) lor last in
                    let new_value = buffer.(last) in
                    output <- new_value :: temp1 :: output;

                    (* Update the dictionary. *)
                    let updated = (index + buffer_offset - 1) mod dict_size in
                    let old_value = dictionary.(updated) in
                    dictionary.(updated) <- new_value;
                    cursor <- (updated + 1) mod dict_size;

                    (* Remove the old value from the map. *)
                    try
                        let old_set = Hashtbl.find value_map old_value in
                        let new_set = IntSet.remove updated old_set in
                        if IntSet.is_empty new_set then
                            Hashtbl.remove value_map old_value
                        else Hashtbl.replace value_map old_value new_set
                    with Not_found -> ();

                    (* Add the new value to the map. *)
                    try
                        let old_set = Hashtbl.find value_map new_value in
                        let new_set = IntSet.add updated old_set in
                        Hashtbl.replace value_map new_value new_set
                    with Not_found ->
                        let new_set = IntSet.singleton updated in
                        Hashtbl.add value_map new_value new_set

                end
            else if buffer_offset = 1 then (* Empty run *)
                begin
                    let temp = cursor lsl dict_size_bits in
                    output <- buffer.(0) :: temp :: output;
                    dictionary.(cursor) <- buffer.(0);
                    let new_set = IntSet.singleton cursor in
                    Hashtbl.add value_map buffer.(0) new_set;
                    cursor <- (cursor + 1) mod dict_size
                end
            else ();
            buffer_offset <- 0

        method private update_run value =
            let f pos = 
                let i = (buffer_offset + pos) mod dict_size in
                value = dictionary.(i)
            in
            let updated = IntSet.filter f buffer_starts in
            buffer.(buffer_offset) <- value;
            buffer_offset <- buffer_offset + 1;
            if IntSet.is_empty updated then self#finish_run
            else buffer_starts <- updated

        method trace value =
            if buffer_offset = 0 then
                self#start_run value
            else if buffer_offset < dict_size - 1 then
                self#update_run value
            else
                begin
                    self#finish_run;
                    self#start_run value
                end

        method private get_value is_prod chan time =
            let delta = time - last_time in
            let max_time = (1 lsl 24) - 1 in
            let t = if delta > max_time then max_time else delta in
            last_time <- time;
            if is_prod then (1 lsl 31) lor (chan lsl 24) lor t
            else (chan lsl 24) lor t

        method trace_produce chan time =
            self#trace (self#get_value true chan time)

        method trace_consume chan time =
            self#trace (self#get_value false chan time)

        method get_output =
            self#finish_run;
            List.rev output

    end
