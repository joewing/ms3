
let dict_size = 65536;;

class compress =
    object (self)

        (* Sliding window dictionary. *)
        val dictionary = Array.make dict_size 0
        val mutable cursor = 0

        (* Mapping of values to positions in the dictionary. *)
        val value_map : (int, int list) Hashtbl.t = Hashtbl.create dict_size

        (* The current input buffer. *)
        val buffer = Array.make dict_size 0
        val mutable buffer_offset = 0
        val mutable buffer_starts : int list = []

        (* The output.
         * Reprsentation is:
         *      [index:16][count:16], [value:32], ...
         *)
        val mutable output : int list = []

        method private show_dict =
            for i = 0 to 16 do
                Printf.printf "%d " dictionary.(i)
            done; Printf.printf "\n"

        method private show_map =
            Hashtbl.iter (fun k -> fun l ->
                Printf.printf "(%d -> " k;
                List.iter (fun v ->
                    Printf.printf "%d " v
                ) l;
                Printf.printf ") "
            ) value_map;
            Printf.printf "\n"

        method private show_stat =
            Printf.printf "dict: "; self#show_dict;
            Printf.printf "map: "; self#show_map

        method private start_run value =
            buffer.(0) <- value;
            buffer_offset <- 1;
            if Hashtbl.mem value_map value then
                (* This is the start of a run. *)
                buffer_starts <- Hashtbl.find value_map value
            else
                (* Not the start of a run. *)
                self#finish_run

        method private finish_run =
            if buffer_offset > 1 then (* Multiple values in this run. *)
                begin

                    (* Output the run and new value. *)
                    let index = List.hd buffer_starts in
                    let last = buffer_offset - 1 in
                    let temp1 = (index lsl 16) lor last in
                    let new_value = buffer.(last) in
                    output <- new_value :: temp1 :: output;

                    (* Update the dictionary. *)
                    let updated = (index + buffer_offset - 1) mod dict_size in
                    let old_value = dictionary.(updated) in
                    dictionary.(updated) <- new_value;
                    cursor <- (updated + 1) mod dict_size;

                    (* Remove the old value from the map. *)
                    if Hashtbl.mem value_map old_value then
                        let old_list = Hashtbl.find value_map old_value in
                        let new_list = List.filter (fun i ->
                            i != updated
                        ) old_list in
                        match new_list with
                        | [] -> Hashtbl.remove value_map old_value
                        | _ -> Hashtbl.replace value_map old_value new_list
                    else ();

                    (* Add the new value to the map. *)
                    if Hashtbl.mem value_map new_value then
                        let old_list = Hashtbl.find value_map new_value in
                        let new_list = updated :: old_list in
                        Hashtbl.replace value_map new_value new_list
                    else
                        let new_list = [updated] in
                        Hashtbl.add value_map new_value new_list

                end
            else if buffer_offset = 1 then (* Empty run *)
                begin
                    let temp = cursor lsl 16 in
                    output <- buffer.(0) :: temp :: output;
                    dictionary.(cursor) <- buffer.(0);
                    Hashtbl.add value_map buffer.(0) [cursor];
                    cursor <- (cursor + 1) mod dict_size
                end
            else ();
            buffer_offset <- 0

        method private update_run value =
            let updated = List.filter (fun pos ->
                let i = (buffer_offset + pos) mod dict_size in
                value = dictionary.(i)
            ) buffer_starts in
            buffer.(buffer_offset) <- value;
            buffer_offset <- buffer_offset + 1;
            match updated with
            | [] -> (* value does not continue this run *)
                self#finish_run;
            | _ -> (* value continues this run *)
                buffer_starts <- updated

        method trace value =
            if buffer_offset = 0 then
                self#start_run value
            else if buffer_offset < dict_size then
                self#update_run value
            else
                begin
                    self#finish_run;
                    self#start_run value
                end

        method get_output =
            self#finish_run;
            List.rev output

    end
