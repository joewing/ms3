open Dram

class pcm =
    object (self)
        inherit dram as super

        method nj_per_read =
            0.0025 *. (float_of_int self#word_size) *. 8.0

        method nj_per_write =
            0.01688 *. (float_of_int self#word_size) *. 8.0

        method nj_static t = 0.0

    end
