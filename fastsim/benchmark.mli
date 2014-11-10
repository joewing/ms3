type 'a stream = SNil | SCons of 'a * (unit -> 'a stream)

type access = char * int * int

class virtual benchmark :
    object
        val mutable index : int
        val mutable last : bool
        method virtual name : string
        method id : int
        method synthetic : bool
        method is_last : bool
        method is_ignored : bool
        method set : string -> string -> unit
        method virtual run : string -> access stream
    end
