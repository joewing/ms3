open Benchmark

class trace :
    object
        val mutable file_name : string
        val mutable index : int
        method id : int
        method run : string -> access stream
        method set : string -> string -> unit
    end
