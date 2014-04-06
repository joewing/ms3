open Subsystem

class fifo =
    object (self)
        inherit subsystem as super

        val mutable min_depth : int = 1
        val mutable read_ptr : int = 0
        val mutable write_ptr : int = 0
        val mutable used : int = 0

        method total_size = depth * word_size

        method set name value = match name with
        | "depth" -> depth <- int_of_string value
        | "min_depth" -> min_depth <- int_of_string value
        | _ -> super#set name value

        method reset m main =
            super#reset m main;
            read_ptr <- 0;
            write_ptr <- 0;
            used <- 0

        method is_full = used = depth

        method is_empty = used = 0

        method produce () =
            if used = depth then
                -1
            else
                let addr = write_ptr * word_size in
                begin
                    write_ptr <- (write_ptr + 1) mod depth;
                    used <- used + 1;
                    super#process 0 true addr word_size
                end

        method consume () =
            if used == 0 then
                -1
            else
                let addr = read_ptr * word_size in
                begin
                    read_ptr <- (read_ptr + 1) mod depth;
                    used <- used - 1;
                    super#process 0 false addr word_size
                end

        method peek offset =
            if used <= offset then
                -1
            else
                let temp = (read_ptr - offset) mod depth in
                let addr = temp * word_size in
                super#process 0 false addr word_size

    end
