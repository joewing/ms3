
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
        val dictionary = Array.make dict_size (-1)
        val mutable cursor = 0

        (* Mapping of values to positions in the dictionary. *)
        val value_map : (int, IntSet.t) Hashtbl.t = Hashtbl.create dict_size

        (* The current input buffer. *)
        val buffer = Array.make dict_size 0
        val mutable buffer_offset = 0
        val buffer_starts = Array.make dict_size (-1)
        val mutable buffer_start_count = 0
        val mutable buffer_start_offset = 0

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
            buffer_start_count <- 0;
            buffer_start_offset <- 0;
            try
                IntSet.iter (fun i ->
                    buffer_starts.(buffer_start_count) <- i;
                    buffer_start_count <- buffer_start_count + 1;
                ) @@ Hashtbl.find value_map value;
                if (Hashtbl.length value_map) = 1 then
                    buffer_start_count <- 1;
            with Not_found -> self#finish_run

        method private finish_run =
            if buffer_offset > 1 then (* Multiple values in this run. *)
                begin

                    (* Output the run and new value. *)
                    let index = buffer_starts.(buffer_start_offset) in
                    let last = buffer_offset - 1 in
                    let temp1 = (index lsl dict_size_bits) lor last in
                    let new_value = buffer.(last) in
                    output <- new_value :: temp1 :: output;

                    (* Update the dictionary. *)
                    let updated = (index + last) mod dict_size in
                    let old_value = dictionary.(updated) in
                    dictionary.(updated) <- new_value;
                    cursor <- (updated + 1) mod dict_size;

                    (* Remove the old value from the map. *)
                    begin
                        try
                            let old_set = Hashtbl.find value_map old_value in
                            let new_set = IntSet.remove updated old_set in
                            if IntSet.is_empty new_set then
                                Hashtbl.remove value_map old_value
                            else Hashtbl.replace value_map old_value new_set
                        with Not_found -> ()
                    end;

                    (* Add the new value to the map. *)
                    begin
                        try
                            let old_set = Hashtbl.find value_map new_value in
                            let new_set = IntSet.add updated old_set in
                            Hashtbl.replace value_map new_value new_set
                        with Not_found ->
                            let new_set = IntSet.singleton updated in
                            Hashtbl.replace value_map new_value new_set
                    end

                end
            else if buffer_offset = 1 then (* Empty run *)
                begin
                    let temp = cursor lsl dict_size_bits in
                    output <- buffer.(0) :: temp :: output;

                    begin
                        try (* Remove the old value from the map. *)
                            let old_value = dictionary.(cursor) in
                            let old_set = Hashtbl.find value_map old_value in
                            let new_set = IntSet.remove cursor old_set in
                            if IntSet.is_empty new_set then
                                Hashtbl.remove value_map old_value
                            else Hashtbl.replace value_map old_value new_set
                        with Not_found -> ()
                    end;

                    (* Update the dictionary. *)
                    dictionary.(cursor) <- buffer.(0);

                    (* Add the new value.
                     * Note that the value can't already exist since
                     * we would have had a run in that case.
                     *)
                    let new_set = IntSet.singleton cursor in
                    Hashtbl.replace value_map buffer.(0) new_set;

                    (* Update the cursor. *)
                    cursor <- (cursor + 1) mod dict_size
                end
            else ();
            buffer_offset <- 0

        method private update_run value =
            let old_start = buffer_starts.(buffer_start_offset) in
            let old_pos = (old_start + buffer_offset) mod dict_size in
            buffer.(buffer_offset) <- value;
            buffer_offset <- buffer_offset + 1;
            if value != dictionary.(old_pos) then
                begin
                    let rec check start index =
                        let pos = (start + index) mod dict_size in
                        if index = buffer_offset then false
                        else if buffer.(index) != dictionary.(pos) then true
                        else check start (index + 1)
                    in
                    let new_offset = ref (buffer_start_offset + 1) in
                    while !new_offset < buffer_start_count
                        && check buffer_starts.(!new_offset) 1 do
                        new_offset := !new_offset + 1
                    done;
                    if !new_offset = buffer_start_count then self#finish_run
                    else buffer_start_offset <- !new_offset
                end
            else ();

        method trace value =
            if buffer_offset = 0 then
                self#start_run value
            else if buffer_offset < dict_size - 2 then
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
