open Main_memory

class ram =
    object (self)
        inherit main_memory as super

        val mutable word_size = 4
        val mutable latency = 100

        method set name value =
            match name with
            | "word_size" -> word_size <- int_of_string value
            | "latency" -> latency <- int_of_string value
            | _ -> super#set name value

        method energy t = 0.0

        method word_size = word_size

        method private process base start write addr size =
            writes <- writes + (if write then 1 else 0);
            reads <- reads + (if write then 0 else 1);
            start + latency

    end
