open Dram

class pcm =
    object (self)
        inherit dram as super

        method nj_per_write =
            0.013733 *. (float_of_int page_size)

        method nj_per_read =
            0.0268 *. (float_of_int page_size)

        method nj_static t = 0.0

    end
