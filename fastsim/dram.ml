open Machine
open Main_memory

type dram_bank = {
    mutable page : int;
    mutable dirty : bool;
    mutable time : float
}

class dram =
    object (self)
        inherit main_memory as super
        val mutable frequency = 400000000.0
        val mutable cas_cycles = 5
        val mutable rcd_cycles = 5
        val mutable rp_cycles = 5
        val mutable wb_cycles = 0
        val mutable page_size = 1024
        val mutable page_count = 65536
        val mutable width = 8
        val mutable burst_size = 4
        val mutable open_page = true
        val mutable ddr = true
        val mutable extra_cycles = 0
        val mutable banks : dram_bank array = Array.make 0 {
            page = -1; dirty = false; time = 0.0
        }

        method private bank_size = page_size * page_count

        method set name value =
            match name with
            | "frequency" -> frequency <- float_of_string value
            | "cas_cycles" -> cas_cycles <- int_of_string value
            | "rcd_cycles" -> rcd_cycles <- int_of_string value
            | "rp_cycles" -> rp_cycles <- int_of_string value
            | "wb_cycles" -> wb_cycles <- int_of_string value
            | "page_size" -> page_size <- int_of_string value
            | "page_count" -> page_count <- int_of_string value
            | "width" -> width <- int_of_string value
            | "burst_size" -> burst_size <- int_of_string value
            | "open_page" -> open_page <- value = "true"
            | "ddr" -> ddr <- value = "true"
            | "extra_cycles" -> extra_cycles <- int_of_string value
            | _ -> super#set name value

        method word_size = width * burst_size

        method reset m main =
            super#reset m main;
            let bank_size = self#bank_size in
            let bank_count = (mach.addr_mask + bank_size) / bank_size in
            banks <- Array.init bank_count (fun _ ->
                { page = -1; dirty = false; time = 0.0 }
            )

        method private process start write addr size =
            writes <- writes + (if write then 1 else 0);
            let mult = mach.frequency /. frequency in
            let start = ((float_of_int start) /. mult) in

            (* Look up the bank. *)
            let bank_index = addr / self#bank_size in
            let bank = banks.(bank_index) in

            (* Make sure the bank is ready for another request. *)
            let mtime = (float_of_int mach.time) /. mult in
            let cycles = max start (bank.time -. mtime) in

            (* Determine how many cycles to use for the burst. *)
            let bsize = float_of_int burst_size in
            let cycles = cycles +. (if ddr then bsize /. 2.0 else bsize) in

            (* Extra time until the page can be used again. *)
            let extra = if not open_page then
                rp_cycles + (if write then wb_cycles else 0)
            else 0
            in

            (* Time for this access. *)
            let page_index = addr / page_size in
            let cycles = cycles +. (float_of_int @@ if not open_page then
                cas_cycles + rcd_cycles
            else if bank.page = page_index then
                cas_cycles
            else if bank.dirty then
                rp_cycles + rcd_cycles + cas_cycles + wb_cycles
            else
                rp_cycles + rcd_cycles + cas_cycles)
            in

            (* Update the bank. *)
            bank.time <- mtime +. cycles +. (float_of_int extra);
            bank.page <- page_index;
            bank.dirty <-
                if bank.page = page_index then (bank.dirty || write)
                else write;

            (* Return the result. *)
            (mult *. cycles +. extra_cycles) |> ceil |> int_of_float


    end
