open Machine
open Base_memory

type cache_policy = LRU | MRU | FIFO | PLRU

type cache_line = {
    mutable tag : int;
    mutable age : int;
    mutable dirty : bool
}

let parse_cache_policy = function
    | "lru" -> LRU
    | "mru" -> MRU
    | "fifo" -> FIFO
    | "plru" -> PLRU
    | s -> failwith ("invalid cache policy: " ^ s)
;;

class cache =
    object (self)
        inherit container as super

        val mutable line_count : int = 1
        val mutable line_size : int = 8
        val mutable associativity : int = 1
        val mutable access_time : int = 1
        val mutable cycle_time : int = 1
        val mutable policy : cache_policy = LRU
        val mutable write_back : bool = true
        val mutable pending : int = 0
        val mutable lines : cache_line array = Array.make 0 {
            tag = -1; age = 0; dirty = false
        }

        method set name value =
            match name with
            | "line_count" -> line_count <- int_of_string value
            | "line_size" -> line_size <- int_of_string value
            | "associativity" -> associativity <- int_of_string value
            | "policy" -> policy <- parse_cache_policy value
            | "write_back" -> write_back <- value = "true"
            | "access_time" -> access_time <- int_of_string value
            | "cycle_time" -> cycle_time <- int_of_string value
            | _ -> super#set name value

        method word_size = line_size

        method private set_size = line_count / associativity

        method reset m main =
            super#reset m main;
            pending <- 0;
            lines <- Array.init line_count (fun _ ->
                { tag = 0; age = 0; dirty = false }
            )

        method finish = max (pending - mach.time) self#next#finish

        method private sum_ages first_line =
            let set_size = self#set_size in
            let rec loop i total =
                if i < associativity then
                    let line_index = first_line + i * set_size in
                    let line = lines.(line_index) in
                    loop (i + 1) (total + line.age)
                else total
            in loop 0 0

        method private update_ages first_line =
            let rec update i =
                if i < associativity then
                    let line_index = first_line + i * self#set_size in
                    let line = lines.(line_index) in
                    let age = line.age in
                    if policy = PLRU then
                        line.age <- 0
                    else line.age <- age + 1;
                    update (i + 1)
            in update 0

        method private find_hit addr =
            let tag = addr land lnot (line_size - 1) in
            let line_addr = addr / line_size in
            let set_size = self#set_size in
            let first_line = line_addr mod set_size in
            let rec loop i =
                if i < associativity then
                    let index = first_line + i * set_size in
                    let line = lines.(index) in
                    if line.tag = tag then
                        Some index
                    else loop (i + 1)
                else None
            in loop 0

        method private find_replacement addr =
            let set_size = self#set_size in
            let line_addr = addr / line_size in
            let first_line = line_addr mod set_size in
            let rec loop i best =
                if i < associativity then
                    let index = first_line + i * set_size in
                    let line = lines.(index) in
                    let ni = i + 1 in
                    match policy with
                    | MRU when line.age < best.age  -> loop ni line
                    | PLRU when line.age = 0        -> loop ni line
                    | LRU when line.age > best.age  -> loop ni line
                    | FIFO when line.age > best.age -> loop ni line
                    | _                             -> loop ni best
                else best
            in loop 1 lines.(first_line)

        method private process_hit base start write line_index =

            (* Update ages. *)
            let set_size = self#set_size in
            let line = lines.(line_index) in
            let line_addr = line.tag / line_size in
            let first_line = line_addr mod set_size in
            if policy = PLRU then
                begin
                    let age_sum = self#sum_ages first_line in
                    if age_sum + 1 = associativity then
                        self#update_ages first_line
                    else ();
                    line.age <- 1
                end
            else if policy <> FIFO then
                line.age <- 0
            else ();

            (* Return access time. *)
            if (not write) || write_back then
                begin
                    line.dirty <- line.dirty || write;
                    start + access_time
                end
            else
                let addr, size = line.tag, line_size in
                let t = self#next#send_request base start true addr size in
                access_time + t

        method private process_miss base start write addr size =
            let line = self#find_replacement addr in
            let tag = addr land lnot (line_size - 1) in
            if (not write) || write_back then
                let t = start + access_time in

                (* Write-back. *)
                let t =
                    if line.dirty then
                        let addr, size = line.tag, line_size in
                        self#next#send_request base t true addr size
                    else t
                in
                line.tag <- tag;
                line.dirty <- write;

                (* Update age. *)
                if policy = PLRU then
                    begin
                        let line_addr = addr / line_size in
                        let first_line = line_addr mod self#set_size in
                        let age_sum = self#sum_ages first_line in
                        if age_sum + 1 = associativity then
                            self#update_ages first_line
                        else ();
                        line.age <- 1
                    end
                else line.age <- 0;

                (* Read the new entry. *)
                if (not write) || size < line_size then
                    self#next#send_request base t false tag line_size
                else t

            else
                let t = self#next#send_request base start true addr size in
                t + access_time

        method private process base start write addr size =

            (* Get earliest time we could process this event. *)
            let t = max start (pending - mach.time) in

            (* Update ages. *)
            if policy <> PLRU then
                let line_addr = addr / line_size in
                let first_line = line_addr mod self#set_size in
                self#update_ages first_line
            else ();

            (* Process the access. *)
            let t = match self#find_hit addr with
            | Some index -> self#process_hit base t write index
            | None -> self#process_miss base t write addr size
            in

            (* Update the pending time. *)
            let temp = max (cycle_time - access_time) 0 in
            pending <- mach.time + t + temp;
            t

    end
