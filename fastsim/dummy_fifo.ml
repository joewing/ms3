open Fifo
open Machine

class dummy_fifo =
    object (self)
        inherit fifo as super

        method is_full = false

        method is_empty = false

        method produce = 1

        method consume = 1

        method peek offset = 1

    end
