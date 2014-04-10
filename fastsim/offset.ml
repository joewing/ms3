open Machine
open Base_memory

class offset =
    object (self)
        inherit transform as super

        val mutable offset : int = 0

        method set name value =
            match name with
            | "value" -> offset <- int_of_string value
            | _ -> super#set name value

        method process start write addr size =
            let addr = (addr + offset) land mach.addr_mask in
            self#bank#send_request start write addr size

        method forward index start write addr size =
            let addr = (addr - offset) land mach.addr_mask in
            self#next#send_request start write addr size

    end
