type 'a stream = SNil | SCons of 'a * (unit -> 'a stream)

type access = char * int * int

class trace :
    object
        val mutable file_name : string
        val mutable index : int
        method id : int
        method run : string -> (char * int * int) stream
        method set : string -> string -> unit
    end
