open Machine
open Base_memory

class shift =
    object (self)
        inherit transform as super

        val mutable offset : int = 0

        method set name value =
            match name with
            | "value" -> offset <- int_of_string value
            | _ -> super#set name value

        method private rotate value count =
            let word_size = self#word_size in
            let word_bits = (log2 word_size) - 1 in
            let word_mask = word_size - 1 in
            let bits = mach.addr_bits - word_bits in
            let count = count mod bits in
            let mask = (1 lsl bits) - 1 in
            let addr = value lsr word_bits in
            let word = value land word_mask in
            let shifted =
                if count >= 0 then
                    let right = bits - count in
                    ((addr lsl count) land mask) lor (addr lsr right)
                else
                    let left = bits + count in
                    ((addr lsl left) land mask) lor (addr lsr -count)
            in (shifted lsl word_bits) lor word

        method private process base start write addr size =
            let addr = self#rotate addr offset in
            self#bank#send_request base start write addr size

        method forward base index start write addr size =
            let addr = self#rotate addr (-offset) in
            self#next#send_request base start write addr size

    end
