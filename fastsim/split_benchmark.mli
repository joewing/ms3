open Benchmark

class split_benchmark :
    object
        inherit benchmark
        val mutable in_port : int
        val mutable out0 : int
        val mutable out1 : int
        method name : string
        method get_parameters : string
        method run : string -> access stream
        method set : string -> string -> unit
    end
