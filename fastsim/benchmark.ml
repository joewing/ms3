type 'a stream = SNil | SCons of 'a * (unit -> 'a stream)

type access = char * int * int

class virtual benchmark =
    object (self)

        val mutable index : int = 0

        method id = index

        method set name value =
            match name with
            | "id" -> index <- int_of_string value
            | _ -> failwith @@ "invalid benchmark parameter: " ^ name

        method virtual run : string -> access stream

    end
