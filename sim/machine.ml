class machine =
    object
        val mutable time : int = 0
        val mutable addr_bits : int = 0
        val mutable frequency : int = 0

        method set name value =
            match name with
            | "addr_bits" -> addr_bits <- int_of_string value
            | "frequency" -> frequency <- int_of_float @@ float_of_string value
            | _ -> ()

        method get_addr_mask = (1 lsl addr_bits) - 1

        method frequency = frequency

        method get_time = time

        method set_time t = time <- t

        method reset () =
            time <- 0

    end

