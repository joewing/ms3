
type machine = {
    mutable time : int;
    mutable addr_bits : int;
    mutable addr_mask : int;
    mutable frequency : float;
}

let create_machine () =
    let default_bits = 32 in
    let default_freq = 1000000000.0 in
    {
        time = 0;
        addr_bits = default_bits;
        addr_mask = (1 lsl default_bits) - 1;
        frequency = default_freq
    }
;;

let set_machine mach name value =
    match name with
    | "addr_bits" ->
        mach.addr_bits <- int_of_string value;
        mach.addr_mask <- (1 lsl mach.addr_bits) - 1
    | "frequency" ->
        mach.frequency <- float_of_string value
    | _ -> ()
;;

let reset_machine mach =
    mach.time <- 0
;;
