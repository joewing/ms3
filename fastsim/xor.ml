open Base_memory

class xor =
    object (self)
        inherit transform as super

        val mutable value : int = 0

        method set name v =
            match name with
            | "value" -> value <- int_of_string v
            | _ -> super#set name v

        method process start write addr size =
            let addr = addr lxor value in
            self#bank#send_request start write addr size

        method forward index start write addr size =
            let addr = addr lxor value in
            self#next#send_request start write addr size

    end
