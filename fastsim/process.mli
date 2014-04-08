type process

type producer = process -> int -> int

type consumer = process -> int -> int

type peeker = process -> int -> int -> int

type runner = string -> Trace.access Trace.stream

val create_process : producer -> consumer -> peeker -> runner ->
                     string -> Base_memory.base_memory -> process

val process_reset : process -> unit

val process_step : process -> int

val process_finish : process -> int

val process_is_done : process -> bool
