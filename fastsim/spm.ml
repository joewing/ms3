open Machine
open Base_memory

class spm =
    object (self)
        inherit container as super

        val mutable word_size : int = 4
        val mutable size_bytes : int = 0
        val mutable access_time : int = 2
        val mutable cycle_time : int = 2
        val mutable pending : int = 0

        method set name value =
            match name with
            | "word_size" -> word_size <- int_of_string value
            | "size" -> size_bytes <- int_of_string value
            | "access_time" -> access_time <- int_of_string value
            | "cycle_time" -> cycle_time <- int_of_string value
            | _ -> super#set name value

        method word_size = word_size

        method reset m main =
            super#reset m main;
            pending <- 0

        method private process_hit base start write addr size =
            pending <- mach.time + start;
            pending <- pending + max (cycle_time - access_time) 0;
            start + access_time

        method private process_miss base start write addr size =
            pending <- mach.time + start;
            self#next#send_request base start write addr size

        method private process base start write addr size =
            let result = max start (pending - mach.time) in
            if addr < size_bytes then
                self#process_hit base result write addr size
            else self#process_miss base result write addr size

    end
