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
        val mutable extra_cycles = 1.0
        val mutable extra_mult = 1.0
        val mutable last_time = 0
        val mutable banks : dram_bank array = Array.make 0 {
            page = -1; dirty = false; time = 0.0
        }

        method nj_per_write = 0.000392 *. (float_of_int page_size)

        method nj_per_read = 0.0012 *. (float_of_int page_size)

        method nj_static t =
            (* Each page must be refreshed once every 64 ms. *)
            let refreshes_per_second = 1.0 /. 0.064 in
            let base = (self#nj_per_write *. t) /. refreshes_per_second in
            let total_pages = page_count * self#bank_count in
            base *. (float_of_int total_pages)

        method energy t =
            let write_energy = self#nj_per_write *. (float_of_int writes) in
            let read_energy = self#nj_per_read *. (float_of_int reads) in
            let static_energy = self#nj_static t in
            let total = write_energy +. read_energy +. static_energy in
            total

        method private bank_size = page_size * page_count

        method private bank_count =
            let bank_size = self#bank_size in
            (mach.addr_mask + bank_size) / bank_size

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
            | "extra" -> extra_cycles <- float_of_string value
            | "multiplier" -> extra_mult <- float_of_string value
            | _ -> super#set name value

        method word_size = width * burst_size

        method reset m main =
            super#reset m main;
            banks <- Array.init self#bank_count (fun _ ->
                { page = -1; dirty = false; time = 0.0 }
            )

        method private process base start write addr size =
            let start = max start (last_time - mach.time) in
            let addr = (addr + base) land mach.addr_mask in
            writes <- writes + (if write then 1 else 0);
            reads <- reads + (if write then 0 else 1);
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
            let delta = cycles -. start in
            let cycles = start +. extra_mult *. delta in
            let result = (mult *. cycles +. extra_cycles)
                       |> ceil |> int_of_float in
            last_time <- mach.time + result;
            result

    end
