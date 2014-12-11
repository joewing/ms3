open Machine
open Subsystem

class fifo =
    object (self)
        inherit subsystem as super

        val mutable min_depth : int = 1
        val mutable bram : bool = true
        val mutable read_ptr : int = 0
        val mutable write_ptr : int = 0
        val mutable used : int = 0

        method total_size = depth * word_size

        method set name value =
            match name with
            | "depth" ->
                depth <- max (int_of_string value) min_depth
            | "bram" ->
                bram <- value = "true"
            | "min_depth" ->
                min_depth <- int_of_string value;
                depth <- max depth min_depth
            | _ -> super#set name value

        method reset m main =
            super#reset m main;
            read_ptr <- 0;
            write_ptr <- 0;
            used <- 0;

        method is_full = used = depth

        method is_empty = used = 0

        method private process start write addr size =
            if bram || depth = 1 then start + 1
            else super#process start write addr size

        method produce =
            if used = depth then
                -1
            else
                let addr = write_ptr * word_size in
                begin
                    write_ptr <- (write_ptr + 1) mod depth;
                    used <- used + 1;
                    let result = self#process 0 true addr word_size in
                    score <- score + result;
                    result
                end

        method consume =
            if used == 0 then
                -1
            else
                begin
                    let addr = read_ptr * word_size in
                    let result = self#process 0 false addr word_size in
                    read_ptr <- (read_ptr + 1) mod depth;
                    used <- used - 1;
                    score <- score + result;
                    result
                end

        method peek offset =
            if used <= offset then
                -1
            else
                let temp = (read_ptr - offset) mod depth in
                let addr = temp * word_size in
                self#process 0 false addr word_size

    end
