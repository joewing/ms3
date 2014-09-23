open Main_memory
open Base_memory

module Int = struct
    type t = int
    let compare = compare
end

module IntMap = Map.Make(Int)

class opt =
    object (self)
        inherit main_memory as super
        val mutable index = 0
        val mutable memories : base_memory IntMap.t = IntMap.empty

        method set name value =
            match name with
            | "index" -> index <- int_of_string value
            | _ -> super#set name value

        method set_memory i = function
            | Some(mem) -> memories <- IntMap.add i mem memories
            | None -> ()

        method private get_memory = IntMap.find index memories

        method word_size = self#get_memory#word_size

        method writes = self#get_memory#writes

        method energy t = self#get_memory#energy t

        method reset m main = self#get_memory#reset m main

        method private process base start write addr size =
            self#get_memory#send_request base start write addr size

    end
