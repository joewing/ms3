type machine = {
    mutable time : int;
    mutable addr_bits : int;
    mutable addr_mask : int;
    mutable frequency : float;
    channel_index : int
}

val create_machine : int -> machine

val set_machine : machine -> string -> string -> unit

val reset_machine : machine -> unit
