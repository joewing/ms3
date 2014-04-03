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

        method word_size = word_size

        method process start write addr size = start + latency

    end
