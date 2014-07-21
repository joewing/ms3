type process

type producer = process -> int -> int

type consumer = process -> int -> int

type peeker = process -> int -> int -> int

type runner = string -> Benchmark.access Benchmark.stream

val create_process : producer -> consumer -> peeker -> runner ->
                     string -> Subsystem.subsystem -> process

val process_reset : process -> unit

val process_step : process -> int

val process_is_done : process -> bool

exception End_simulation
