open Base_memory

class split =
    object (self)
        inherit container as super

        val mutable offset : int = 0
        val mutable bank0 : base_memory option = None
        val mutable bank1 : base_memory option = None

        method set name value =
            match name with
            | "offset" -> offset <- int_of_string value
            | _ -> super#set name value

        method set_bank0 b =
            bank0 <- b;
            self#bank0#set_parent (self :> base_memory)

        method set_bank1 b =
            bank1 <- b;
            self#bank1#set_parent (self :> base_memory)

        method bank0 =
            match bank0 with
            | Some b -> b
            | None -> failwith "invalid bank0"

        method bank1 =
            match bank1 with
            | Some b -> b
            | None -> failwith "invalid bank1"

        method reset m main =
            super#reset m main;
            self#bank0#reset m main;
            self#bank1#reset m main

        method finish =
            let bt = max (self#bank0#finish) (self#bank1#finish) in
            max self#next#finish bt

        method process start write addr size =
            let mask = self#machine#addr_mask in
            let last = (addr + size - 1) land mask in
            let result = start in
            if addr > last then
                let first_size = mask - addr + 1 in
                let result = self#do_process result addr first_size write in
                self#do_process result 0 (last + 1) write
            else
                self#do_process result addr size write

        method private do_process start addr size write =
            let last = (addr + size - 1) land self#machine#addr_mask in
            let start =
                if addr < offset then
                    let temp_size =
                        if last < offset then size
                        else offset - addr in
                    send_request (self#bank0) start write addr temp_size
                else start
            in
            if last >= offset then
                let temp_addr, temp_size = 
                    if addr >= offset then (addr - offset, size)
                    else (0, last - offset + 1) in
                send_request (self#bank1) start write temp_addr temp_size
            else start

        method forward index start write addr size =
            let addr =
                match index with
                | 0 -> addr
                | 1 -> (addr + offset) land self#machine#addr_mask
                | _ -> failwith "invalid"
            in send_request self#next start write addr size

    end
