type simulator

val create_simulator : string -> Model.model -> simulator

val run_simulator : simulator -> (string * int) list
