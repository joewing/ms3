open Machine
open Subsystem

class fifo =
    object (self)
        inherit subsystem as super

        val mutable min_depth : int = 1
        val mutable bram : bool = false
        val mutable read_ptr : int = 0
        val mutable write_ptr : int = 0
        val mutable used : int = 0
        val mutable min_produce_time : int = 0
        val mutable min_consume_time : int = 0

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
            min_produce_time <- 0;
            min_consume_time <- 0

        method is_full = used = depth

        method is_empty = used = 0

        method finish =
            let t = max min_produce_time min_consume_time in
            max super#finish (t - mach.time)

        method private process start write addr size =
            let result =
                if bram || depth = 1 then
                    start + 1
                else
                    super#process start write addr size
            in
                score <- score + result;
                result

        method private read_next start =
            let start = max start (min_consume_time - mach.time) in
            let addr = read_ptr * word_size in
            let t = self#process start false addr word_size in
            min_consume_time <- mach.time + t;
            read_ptr <- (read_ptr + 1) mod depth

        method produce =
            if used = depth then
                -1
            else
                let start = max 0 (min_produce_time - mach.time) in
                let addr = write_ptr * word_size in
                begin
                    write_ptr <- (write_ptr + 1) mod depth;
                    used <- used + 1;
                    let t = self#process start true addr word_size in
                    min_produce_time <- mach.time + t;
                    if used = 1 then
                        self#read_next t
                    else ();
                    max 1 start
                end

        method consume =
            if used == 0 then
                -1
            else
                begin
                    let result = max 1 (min_consume_time - mach.time) in
                    if used > 1 then
                        self#read_next result
                    else ();
                    used <- used - 1;
                    result
                end

        method peek offset =
            if used <= offset then
                -1
            else
                let start = max 0 (min_consume_time - mach.time) in
                let temp = (read_ptr - offset) mod depth in
                let addr = temp * word_size in
                self#process start false addr word_size

    end
