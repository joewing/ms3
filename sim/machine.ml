class machine =
    object
        val mutable time : int = 0
        val mutable addr_bits : int = 0
        val mutable frequency : float = 0.0

        method set name value =
            match name with
            | "addr_bits" -> addr_bits <- int_of_string value
            | "frequency" -> frequency <- float_of_string value
            | _ -> ()

        method addr_bits = addr_bits

        method addr_mask = (1 lsl addr_bits) - 1

        method frequency = frequency

        method time = time

        method set_time t = time <- t

        method reset () =
            time <- 0

    end

