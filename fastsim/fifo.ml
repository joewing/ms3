open Machine
open Subsystem

class fifo =
    object (self)
        inherit subsystem as super

        val mutable min_depth : int = 1
        val mutable read_ptr : int = 0
        val mutable write_ptr : int = 0
        val mutable used : int = 0
        val mutable min_time : int = 0

        method total_size = depth * word_size

        method set name value =
            match name with
            | "depth" ->
                depth <- max (int_of_string value) min_depth
            | "min_depth" ->
                min_depth <- int_of_string value;
                depth <- max depth min_depth
            | _ -> super#set name value

        method reset m main =
            super#reset m main;
            read_ptr <- 0;
            write_ptr <- 0;
            used <- 0;
            min_time <- 0

        method is_full = used = depth

        method is_empty = used = 0

        method process start write addr size =
            if depth = 1 then
                let result = start + 1 in
                begin
                    score <- score + result;
                    min_time <- mach.time + result;
                    result
                end
            else
                let result = super#process start write addr size in
                begin
                    min_time <- mach.time + result;
                    result
                end

        method produce =
            if used = depth then
                -1
            else if mach.time < min_time then
                min_time - mach.time
            else
                let addr = write_ptr * word_size in
                begin
                    write_ptr <- (write_ptr + 1) mod depth;
                    used <- used + 1;
                    self#process 0 true addr word_size
                end

        method consume =
            if used == 0 then
                -1
            else if mach.time < min_time then
                min_time - mach.time
            else
                let addr = read_ptr * word_size in
                begin
                    read_ptr <- (read_ptr + 1) mod depth;
                    used <- used - 1;
                    self#process 0 false addr word_size
                end

        method peek offset =
            if used <= offset then
                -1
            else
                let temp = (read_ptr - offset) mod depth in
                let addr = temp * word_size in
                self#process 0 false addr word_size

    end
