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
            page = -1; dirty = false; time = 0.0
        }

        method set name value = match name with
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

        method finish = max (pending - self#machine#time) self#next#finish

        method private sum_ages first_line =
            let set_size = self#set_size in
            let rec loop i total =
                if i < associativity then
                    let line_index = first_line + i * set_size in
                    let line = lines.(line_index) in
                    loop (i - 1) (total + line.age)
                else total
            in loop 0 0

        method private update_ages () =
            let rec update i =
                if i < associativity then
                    let line_index = first_line + i * self#set_size in
                    let line = lines.(line_index) in
                    let age = line.age in
                    if policy = PRLU then
                        line.age <- 0
                    else
                        line.age <- age + increment;
                    update_ages (i + 1)
            in update_ages 0

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
                    | MRU ->
                            if line.age < best.age then
                                loop ni line
                            else loop ni best
                    | PLRU ->
                            if line.age = 0 then
                                loop ni line
                            else
                                loop ni best
                    | _ ->
                            if line.age > best.age then
                                loop ni line
                            else
                                loop ni best
                else best
            in loop 1 lines.(first_line)

        method private process_hit start line =
            let set_size = self#set_size in
            let line_addr = addr / line_size in
            let first_line = line_addr mod set_size in
            if policy = PLRU then
                begin
                    let age_sum = sum_ages first_line in
                    if age_sum + 1 = associativity then
                        self#update_ages ()
                    else ();
                    line.age <- 1
                end
            else if policy <> FIFO then
                line.age <- 0
            else ();
            if (not write) || write_back then
                begin
                    line.dirty <- line.dirty || write;
                    start + access_time
                end
            else
                let addr, size = line.tag, line_size in
                let t = send_request self#next start true addr size in
                access_time + t

        method process start write addr size =
            let tag = addr land lnot (line_size - 1) in
            let line_addr = addr / line_size in
            let first_line = line_addr mod set_size in

            (* Update ages. *)
            if policy = PLRU then
                self#update_ages ();

            (* Check if this address is in the cache. *)
            let to_replace = ref lines.(first_list) in
            let rec check i =
                if i < associativity then
                    let line_index = first_line + i * self#set_size in
                    let line = lines.(line_index) in
                    if tag = line.tag then
                        self#process_hit start line
                    else
                        self#process_miss
                    else if policy = MRU then
                        if line.age < to_replace.age then
                            to_replace := line
                        else ()
                    else if policy = PLRU then
                        if line.age = 0 then
                            to_replace := line
                        else ()
                    else
                        if line.age > to_replace.age then
                            to_replace := line
                        else ();


    end
