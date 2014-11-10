type process

type producer = process -> int -> int

type consumer = process -> int -> int

type peeker = process -> int -> int -> int

type runner = string -> Benchmark.access Benchmark.stream

val create_process : producer -> consumer -> peeker -> runner ->
                     string -> Subsystem.subsystem -> bool -> process

val trace_produce : process -> int -> int -> unit

val trace_consume : process -> int -> int -> unit

val get_trace : process -> int list

val process_reset : process -> unit

val process_step : process -> int

val process_is_done : process -> bool

exception End_simulation
