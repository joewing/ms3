open Benchmark

class split_benchmark =
    object (self)
        inherit benchmark as super

        val mutable in_port : int = -1
        val mutable out0 : int = -1
        val mutable out1 : int = -1

        method name = "split"

        method get_parameters =
            Printf.sprintf "\"in\": %d, \"out0\": %d, \"out1\": %d"
                           in_port out0 out1

        method set name value =
            match name with
            | "in" -> in_port <- int_of_string value
            | "out0" -> out0 <- int_of_string value
            | "out1" -> out1 <- int_of_string value
            | _ -> super#set name value

        method run directory =
            let access_list = [
                ('C', in_port, 0); ('O', out0, out1);
                ('C', in_port, 0); ('O', out1, out0);
            ] in
            let rec process lst () =
                match lst with
                | temp :: rest  -> SCons (temp, process rest)
                | []            -> process access_list ()
            in
            if last then SNil
            else process access_list ()
    end
