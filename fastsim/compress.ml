open Bigarray

let dict_size_bits = 16;;
let dict_size = 1 lsl dict_size_bits;;
let dict_mask = dict_size - 1;;

module Int = struct
    type t = int
    let compare = compare
end

module IntSet = Set.Make(Int)

let create_dictionary () =
    let result = Array1.create int c_layout dict_size in
    Array1.fill result (-1);
    result
;;

class compress =
    object (self)

        (* Sliding window dictionary. *)
        val dictionary = create_dictionary ()
        val mutable cursor = 0

        (* Mapping of values to positions in the dictionary. *)
        val value_map : (int, IntSet.t) Hashtbl.t = Hashtbl.create dict_size

        (* The current input buffer. *)
        val buffer = Array1.create int c_layout dict_size
        val mutable buffer_offset = 0
        val buffer_starts = Array1.create int c_layout dict_size
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
            Array1.unsafe_set buffer 0 value;
            buffer_offset <- 1;
            buffer_start_count <- 0;
            buffer_start_offset <- 0;
            try
                let starts = Hashtbl.find value_map value in
                if (Hashtbl.length value_map) = 1 then
                    begin
                        buffer_start_count <- 1;
                        Array1.unsafe_set buffer_starts 0 0
                    end
                else begin
                    IntSet.iter (fun i ->
                        Array1.unsafe_set buffer_starts buffer_start_count i;
                        buffer_start_count <- buffer_start_count + 1;
                    ) starts;
                    (* Limit the number of starting positions to check. *)
                    buffer_start_count <- min 64 buffer_start_count
                end
            with Not_found -> self#finish_run

        method private finish_run =
            if buffer_offset > 1 then (* Multiple values in this run. *)
                begin

                    (* Output the run and new value. *)
                    let index =
                        Array1.unsafe_get buffer_starts buffer_start_offset
                    in
                    let last = buffer_offset - 1 in
                    let temp1 = (index lsl dict_size_bits) lor last in
                    let new_value = Array1.unsafe_get buffer last in
                    output <- new_value :: temp1 :: output;

                    (* Update the dictionary. *)
                    let updated = (index + last) land dict_mask in
                    let old_value = Array1.unsafe_get dictionary updated in
                    Array1.unsafe_set dictionary updated new_value;
                    cursor <- (updated + 1) land dict_mask;

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
                    let temp1 = Array1.unsafe_get buffer 0 in
                    let temp2 = cursor lsl dict_size_bits in
                    output <- temp1 :: temp2 :: output;

                    begin
                        try (* Remove the old value from the map. *)
                            let old_value =
                                Array1.unsafe_get dictionary cursor
                            in
                            let old_set = Hashtbl.find value_map old_value in
                            let new_set = IntSet.remove cursor old_set in
                            if IntSet.is_empty new_set then
                                Hashtbl.remove value_map old_value
                            else Hashtbl.replace value_map old_value new_set
                        with Not_found -> ()
                    end;

                    (* Update the dictionary. *)
                    Array1.unsafe_set dictionary cursor @@
                        Array1.unsafe_get buffer 0;

                    (* Add the new value.
                     * Note that the value can't already exist since
                     * we would have had a run in that case.
                     *)
                    let new_set = IntSet.singleton cursor in
                    Hashtbl.replace value_map
                        (Array1.unsafe_get buffer 0) new_set;

                    (* Update the cursor. *)
                    cursor <- (cursor + 1) land dict_mask
                end
            else ();
            buffer_offset <- 0

        method private update_run value =
            let old_start =
                Array1.unsafe_get buffer_starts buffer_start_offset
            in
            let old_pos = (old_start + buffer_offset) land dict_mask in
            Array1.unsafe_set buffer buffer_offset value;
            buffer_offset <- buffer_offset + 1;
            if value != Array1.unsafe_get dictionary old_pos then
                begin
                    let check start =
                        let index = ref (buffer_offset - 1) in
                        let pos = ref ((start + !index) land dict_mask) in
                        let result = ref false in
                        while (!index != 0) && (not !result) do
                            let a = Array1.unsafe_get buffer !index in
                            let b = Array1.unsafe_get dictionary !pos in
                            result := a != b;
                            index := !index - 1;
                            pos := if !pos = 0 then dict_mask else !pos - 1
                        done; !result
                    in
                    let new_offset = ref (buffer_start_offset + 1) in
                    while !new_offset < buffer_start_count &&
                        check (Array1.unsafe_get buffer_starts !new_offset)
                    do
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
