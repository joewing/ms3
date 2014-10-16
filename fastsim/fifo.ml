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

        val mutable prod_count : int = 0
        val mutable last_prod_time : int = 0
        val mutable mean_prod_time : float = 0.0
        val mutable m2_prod_time : float = 0.0

        val mutable cons_count : int = 0
        val mutable last_cons_time : int = 0
        val mutable mean_cons_time : float = 0.0
        val mutable m2_cons_time : float = 0.0

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

        method get_item_count = max prod_count cons_count

        method get_produce_time = last_prod_time

        method get_consume_time = last_cons_time

        method get_produce_variance =
            if prod_count < 2 then 0.0
            else m2_prod_time /. (float_of_int (prod_count - 1))

        method get_consume_variance =
            if cons_count < 2 then 0.0
            else m2_cons_time /. (float_of_int (cons_count - 1))

        method register_produce =
            prod_count <- prod_count + 1;
            let it = mach.time - last_prod_time in
            let t = float_of_int it in
            let n = float_of_int prod_count in
            let delta = t -. mean_prod_time in
            last_prod_time <- mach.time;
            mean_prod_time <- mean_prod_time +. delta /. n;
            m2_prod_time <- m2_prod_time +. delta *. (t -. mean_prod_time);
            if mach.channel_index = index then
                Printf.printf "%d\n" it
            else ()

        method register_consume =
            cons_count <- cons_count + 1;
            let it = mach.time - last_cons_time in
            let t = float_of_int it in
            let n = float_of_int cons_count in
            let delta = t -. mean_cons_time in
            last_cons_time <- mach.time;
            mean_cons_time <- mean_cons_time +. delta /. n;
            m2_cons_time <- m2_cons_time +. delta *. (t -. mean_cons_time);
            if mach.channel_index = index then
                Printf.printf "%d\n" it
            else ()

        method reset m main =
            super#reset m main;
            read_ptr <- 0;
            write_ptr <- 0;
            used <- 0;
            min_produce_time <- 0;
            min_consume_time <- 0;
            prod_count <- 0;
            last_prod_time <- 0;
            mean_prod_time <- 0.0;
            m2_prod_time <- 0.0;
            cons_count <- 0;
            last_cons_time <- 0;
            mean_cons_time <- 0.0;
            m2_cons_time <- 0.0

        method consume_time = max mach.time min_consume_time

        method produce_time = max mach.time min_produce_time

        method is_full = used = depth

        method is_empty = used = 0

        method private process start write addr size =
            if bram || depth = 1 then
                start + 1
            else
                super#process start write addr size

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
                    let result = max 1 start in
                    score <- score + result;
                    self#register_produce;
                    result
                end

        method consume =
            if used == 0 then
                -1
            else
                begin
                    let result = max 1 (min_consume_time - mach.time) in
                    if used > 1 then
                        self#read_next 0
                    else ();
                    used <- used - 1;
                    score <- score + result;
                    self#register_consume;
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
