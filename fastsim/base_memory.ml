open Machine

class virtual base_memory =
    object (self)
        val mutable mach : machine = create_machine ()

        method set (name : string) (value : string) : unit =
            failwith @@ "invalid argument: (" ^ name ^ " " ^ value ^ ")"

        method reset (m : machine) (main : base_memory) : unit = mach <- m

        method finish : int = 0

        method virtual word_size : int

        method virtual writes : int

        method private virtual process : int -> bool -> int -> int -> int

        method forward (index : int) (start : int) (write : bool)
                       (addr : int) (size : int) : int =
            failwith "invalid"

        method set_parent (p : base_memory) : unit = failwith "invalid"

        method send_request (start : int) (write : bool)
                            (addr : int) (size : int) : int =
            let word_size = self#word_size in
            let word_mask = word_size - 1 in
            let addr_mask = mach.addr_mask in
            let offset = addr land word_mask in
            let current = ref addr in
            let left = ref size in
            let time = ref start in
            if offset <> 0 then
                begin
                    let first_size = min (word_size - offset) size in
                    time := self#process !time write addr first_size;
                    current := (addr + first_size) land addr_mask;
                    left := !left - first_size;
                end;
            while !left > 0 do
                let temp_size = min word_size !left in
                time := self#process !time write !current temp_size;
                current := (!current + temp_size) land addr_mask;
                left := !left - temp_size
            done;
            !time

    end

class virtual container =
    object (self)
        inherit base_memory as super

        val mutable next : base_memory option = None

        method set_next m = next <- m

        method next =
            match next with
            | Some m -> m
            | None   -> failwith "no next"

        method set_parent p = self#next#set_parent p

        method writes = self#next#writes

        method word_size = self#next#word_size

        method reset m main =
            super#reset m main;
            match next with
            | Some mem -> mem#reset m main
            | None   ->
                    main#reset m main;
                    next <- Some main

        method finish = self#next#finish

    end

class join =
    object (self)
        inherit base_memory as super

        val mutable index : int = 0
        val mutable parent : base_memory option = None

        method set_parent p = parent <- Some p

        method parent =
            match parent with
            | Some p -> p
            | None -> failwith "no parent"

        method word_size = self#parent#word_size

        method writes = self#parent#writes

        method process start write addr size =
            self#parent#forward index start write addr size

    end

class virtual transform =
    object (self)
        inherit container as super

        val mutable bank : base_memory option = None

        method set_bank m =
            bank <- m;
            self#bank#set_parent (self :> base_memory)

        method bank =
            match bank with
            | Some m -> m
            | None -> failwith "no bank"

        method reset m main =
            super#reset m main;
            self#bank#reset m main

        method finish =
            max super#finish self#bank#finish
    end

let log2 (n : int) : int =
    let rec f i r =
        if i > 0 then
            f (i lsr 1) (r + 1)
        else r
    in f n 0
;;
