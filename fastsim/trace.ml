open Str

type 'a stream = SNil | SCons of 'a * (unit -> 'a stream)

let parse_hex s = int_of_string @@ "0x" ^ s;;

class trace =
    object (self)

        val re = regexp "\\([RWMIPCKX]\\)\\([0-9a-fA-F]+\\):\\([0-9a-fA-F]+\\)"

        val mutable index : int = 0
        val mutable file_name : string = "trace"

        method id = index

        method set name value =
            match name with
            | "id" -> index <- int_of_string value
            | "name" -> file_name <- value ^ ".trace"
            | _ -> failwith @@ "invalid trace parameter: " ^ name

        method run directory =
            let full_path = directory ^ "/" ^ file_name in
            try
                let inc = open_in full_path in
                let rec process_file () =
                    try
                        let line = input_line inc in
                        let rec process_line pos () =
                            try
                                let _ = search_forward re line pos in
                                let access = matched_group 1 line
                                and addr = parse_hex @@ matched_group 2 line
                                and size = parse_hex @@ matched_group 3 line in
                                let result = (access.[0], addr, size) in
                                SCons (result, process_line @@ match_end ())
                            with Not_found -> process_file ()
                        in process_line 0 ()
                    with End_of_file -> SNil
                in process_file ()
            with Not_found -> failwith @@ "could not open " ^ full_path

    end
