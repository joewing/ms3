open Fifo

class dummy_fifo =
    object (self)
        inherit fifo as super

        method produce = 1

        method consume = 1

        method peek offset = 1

    end
