open Base_memory

class spm =
    object (self)
        inherit container as super

        val mutable word_size : int = 0
        val mutable size : int = 0
        val mutable access_time : int = 0
        val mutable cycle_time : int = 0
        val mutable pending : int = 0

        method set name value = match name with
        | "word_size" -> word_size <- int_of_string value
        | "size" -> size <- int_of_string value
        | "access_time" -> access_time <- int_of_string value
        | "cycle_time" -> cycle_time <- int_of_string value
        | _ -> super#set name value

        method get_word_size = word_size

        method reset m main =
            super#reset m main;
            pending <- 0

        method finish = max (pending - mach#get_time) self#get_next#finish

        method private process_hit start write addr size =
            let offset = addr mod word_size in
            let count = (size + word_size + offset - 1) / word_size in
            pending <- mach#get_time + start;
            pending <- pending + max (cycle_time - access_time) 0;
            start + (count - 1) * cycle_time + access_time

        method private process_miss start write addr size =
            pending <- mach#get_time + start;
            send_request self#get_next start write addr size

        method private process_hit_miss start write addr size =
            let last_addr = (addr + size) land mach#get_addr_mask in
            let msize = size - last_addr + 1 in
            let count = (last_addr + word_size) / word_size in
            let result = start + (count - 1) * cycle_time + access_time in
            pending <- mach#get_time + result;
            pending <- pending + max (cycle_time - access_time) 0;
            send_request self#get_next result write addr msize

        method private process_miss_hit start write addr size =
            let hsize = size - addr in
            let offset = addr mod word_size in
            let count = (hsize + word_size + offset - 1) / word_size in
            let result = start + (count - 1) * cycle_time + access_time in
            pending <- mach#get_time + result;
            pending <- pending + max (cycle_time - access_time) 0;
            send_request self#get_next result write size (size - hsize)
 
        method process start write addr size =
            let result = max start (pending - mach#get_time) in
            let last_addr = (addr + size) land mach#get_addr_mask in
            if addr < size && last_addr <= size then
                (* Completely hits in the scratchpad. *)
                self#process_hit result write addr size
            else if addr >= size && last_addr > size then
                (* Completely misses the scratchpad. *)
                self#process_miss result write addr size
            else if addr > size && last_addr < size then
                (* First part hits, second part misses. *)
                self#process_hit_miss result write addr size
            else
                (* First part misses, second part hits. *)
                self#process_miss_hit result write addr size

    end
