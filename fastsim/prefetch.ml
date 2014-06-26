open Machine
open Base_memory

class prefetch =
    object (self)
        inherit container as super

        val mutable stride : int = 0
        val mutable time : int = 0

        method set name value =
            match name with
            | "stride" -> stride <- int_of_string value
            | _ -> super#set name value

        method reset m main =
            super#reset m main;
            time <- 0

        method finish = max (time - mach.time) super#finish

        method private process base start write addr size =
            let result = max start (time - mach.time) in
            let result = self#next#send_request base result write addr size in
            begin
                if write then
                    time <- 0
                else
                    let addr = (addr + stride) land mach.addr_mask in
                    let t = self#next#send_request base result write addr 1 in
                    time <- mach.time + t
            end;
            result

    end
