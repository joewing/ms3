open Base_memory
open Machine

class subsystem =
    object (self)

        val mutable mach : machine = create_machine ()
        val mutable index : int = 0
        val mutable word_size : int = 4
        val mutable depth : int = 0
        val mutable offset : int = 0
        val mutable score : int = 0
        val mutable next : base_memory option = None

        method id = index

        method set name value =
            match name with
            | "id" -> index <- int_of_string value
            | "depth" -> depth <- int_of_string value
            | "word_size" -> word_size <- int_of_string value
            | _ -> failwith @@ "invalid argument: (" ^ name ^ " " ^ value ^ ")"

        method set_next m = next <- m

        method next =
            match next with
            | Some m -> m
            | None -> failwith "no next"

        method word_size = word_size

        method set_offset o = offset <- o

        method total_size = depth * word_size

        method score = score

        method reset m main =
            score <- 0;
            mach <- m;
            match next with
            | Some mem -> mem#reset m main
            | None ->
                main#reset m main;
                next <- Some main

        method process start write addr size =
            let result = self#next#send_request offset start write addr size in
            score <- score + result;
            result

        method finish =
            let result = self#next#finish in
            score <- score + result;
            result

    end
