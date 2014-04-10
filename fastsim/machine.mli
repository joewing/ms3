type machine = {
    mutable time : int;
    mutable addr_bits : int;
    mutable addr_mask : int;
    mutable frequency : float;
}

val create_machine : unit -> machine

val set_machine : machine -> string -> string -> unit

val reset_machine : machine -> unit
