open Machine

class virtual base_memory =
    object (self)
        val mutable mach : machine = new machine

        method is_fifo = false

        method set (name : string) (value : string) : unit =
            failwith @@ "invalid argument: (" ^ name ^ " " ^ value ^ ")"

        method machine = mach

        method reset (m : machine) (main : base_memory) = mach <- m

        method finish = 0

        method virtual word_size : int

        method virtual process : int -> bool -> int -> int -> int

        method id : int = failwith "invalid"

        method set_offset (offset: int) : unit = failwith "invalid"

        method total_size : int = failwith "invalid"

        method consume () : int = failwith "invalid"

        method produce () : int = failwith "invalid"

        method peek (offset: int) : int = failwith "invalid"

    end

class container =
    object (self)
        inherit base_memory as super

        val mutable next : base_memory option = None

        method set_next m = next <- m

        method next = match next with
        | Some m -> m
        | None   -> failwith "no next"

        method word_size = self#next#word_size

        method reset m main =
            super#reset m main;
            match next with
            | Some mem -> mem#reset m main
            | None   ->
                    main#reset m main;
                    next <- Some main

        method finish = self#next#finish

        method process = self#next#process

    end

let send_request mem start write addr size =
    let word_size = mem#word_size in
    let word_mask = word_size - 1 in
    let addr_mask = mem#machine#addr_mask in
    let offset = addr land word_mask in
    let current_addr = ref addr in
    let left = ref size in
    let time = ref start in
    if offset <> 0 then
        let first_size = min (word_size - offset) size in
        time := mem#process !time write addr first_size;
        current_addr := (addr + first_size) land addr_mask;
        left := !left - first_size;
    else ();
    while !left > 0 do
        let temp_size = min word_size size in
        time := mem#process !time write !current_addr temp_size;
        current_addr := (!current_addr + temp_size) land addr_mask;
        left := !left - temp_size
    done;
    !time
;;
