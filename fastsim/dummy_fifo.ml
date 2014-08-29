open Fifo
open Machine

class dummy_fifo =
    object (self)
        inherit fifo as super

        method is_full = false

        method is_empty = false

        method produce =
            super#register_produce;
            1

        method consume =
            super#register_consume;
            1

        method peek offset = 1

    end
