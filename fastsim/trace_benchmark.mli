open Benchmark

class trace_benchmark :
    object
        inherit benchmark
        val mutable file_name : string
        val mutable index : int
        method name : string
        method synthetic : bool
        method run : string -> access stream
        method set : string -> string -> unit
    end
