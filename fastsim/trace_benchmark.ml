open Benchmark

let read_hex ch =
    let rec helper value =
        let c = input_char ch in
        if c >= '0' && c <= '9' then
            helper @@ value * 16 + (int_of_char c) - (int_of_char '0')
        else if c >= 'a' && c <= 'f' then
            helper @@ value * 16 + (int_of_char c) - (int_of_char 'a') + 10
        else if c >= 'A' && c <= 'F' then
            helper @@ value * 16 + (int_of_char c) - (int_of_char 'a') + 10
        else value
    in helper 0
;;

let read_access ch =
    let t = input_char ch in
    let addr = read_hex ch in
    let size = read_hex ch in
    (t, addr, size)
;;

class trace_benchmark =
    object (self)
        inherit benchmark as super

        val mutable file_name : string = "trace"

        method name = "trace"

        method synthetic = false

        method set name value =
            match name with
            | "name" -> file_name <- value ^ ".trace"
            | _ -> super#set name value

        method run directory =
            let full_path = directory ^ "/" ^ file_name in
            try
                let inc = open_in full_path in
                let rec process_file () =
                    try
                        let result = read_access inc in
                        SCons (result, process_file)
                    with End_of_file ->
                        if super#is_last then
                            begin
                                Printf.printf "EOF %s\n" file_name;
                                SNil
                            end
                        else
                            begin
                                Printf.printf "rewind %s\n" file_name;
                                seek_in inc 0;
                                process_file ()
                            end
                in process_file ()
            with Not_found -> failwith @@ "could not open " ^ full_path

    end
