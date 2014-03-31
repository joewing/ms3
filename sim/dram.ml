open Main_memory

type dram_bank = {
    mutable page : int;
    mutable dirty : bool;
    mutable time : int
}

class dram =
    object (self)
        inherit main_memory as super
        val mutable frequency = 0
        val mutable cas_cycles = 0
        val mutable rcd_cycles = 0
        val mutable rp_cycles = 0
        val mutable wb_cycles = 0
        val mutable page_size = 0
        val mutable page_count = 0
        val mutable width = 0
        val mutable burst_size = 0
        val mutable open_page = false
        val mutable ddr = false
        val mutable banks : dram_bank array = Array.create 0 {
            page = 0; dirty = false; time = 0
        }

        method private get_bank_size = page_size * page_count

        method private get_bank_count =
            let bank_size = self#get_bank_size in
            (mach#get_addr_mask + bank_size) / bank_size

        method private get_multiplier =
            (mach#frequency + frequency - 1) / frequency

        method set name value = match name with
        | "frequency" -> frequency <- int_of_string value
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
        | _ -> super#set name value

        method get_word_size = width * burst_size

        method reset m main =
            super#reset m main;
            banks <- Array.create self#get_bank_count {
                page = 0; dirty = false; time = 0
            }

        method process start write addr size =
            let multiplier = self#get_multiplier in
            let delta = ref ((start + multiplier) / multiplier) in
            let bsize = burst_size * width in
            let last_addr = addr + size - 1 in
            let current_addr = ref addr in
            while !current_addr <= last_addr do
                let temp = addr - (addr mod bsize) + bsize in
                let is_last = temp >= last_addr in
                current_addr := !current_addr land mach#get_addr_mask;
                delta := self#do_process !delta write
                                         !current_addr is_last;
                current_addr := temp
            done;
            !delta * multiplier

        method private do_process start write addr is_last =

            (* Look up the bank. *)
            let bank_size = page_size * page_count in
            let bank_index = addr / bank_size in
            let bank = banks.(bank_index) in

            (* Make sure the bank is ready for another request. *)
            let mtime = mach#get_time / self#get_multiplier in
            let base = max start (bank.time - mtime) in

            (* Determine how many cycles to use for the burst. *)
            let burst_cycles = if ddr then
                (burst_size + 1) / 2
            else
                burst_size 
            in

            (* Extra time until the page can be used again. *)
            let extra = if not open_page then
                rp_cycles + (if write then wb_cycles else 0)
            else
                0
            in

            (* Time for this access. *)
            let page_index = addr / page_size in
            let t = base + burst_cycles + if not open_page then
                cas_cycles + rcd_cycles
            else if bank.page = page_index then
                cas_cycles
            else if bank.dirty then
                rp_cycles + rcd_cycles + cas_cycles + wb_cycles
            else
                rp_cycles + rcd_cycles + cas_cycles
            in

            (* Update the bank. *)
            bank.time <- mtime + t + extra;
            bank.page <- page_index;
            bank.dirty <- if not open_page then
                false
            else if bank.page = page_index then
                bank.dirty || write
            else
                write
            ;

            (* Return the result. *)
            t

    end
