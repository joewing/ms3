type 'a stream = SNil | SCons of 'a * (unit -> 'a stream)

type access = char * int * int

class virtual benchmark :
    object
        val mutable index : int
        method id : int
        method set : string -> string -> unit
        method virtual run : string -> access stream
    end
