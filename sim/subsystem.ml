open Base_memory

class subsystem =
    object (self)
        inherit base_memory as super

        val mutable index : int = 0
        val mutable word_size : int = 4
        val mutable size : int = 16777216
        val mutable next : base_memory option = None
        val mutable offset : int = 0
        val mutable score : int = 0

        method id = index

        method set name value = match name with
        | "id" -> index <- int_of_string value
        | "size" -> size <- int_of_string value
        | "word_size" -> word_size <- int_of_string value
        | _ -> super#set name value

        method word_size = word_size

        method set_offset o = offset <- o

        method total_size = size

        method next = match next with
            | Some m -> m
            | None -> failwith "no next"

        method set_next n = next <- n

        method reset m main =
            super#reset m main;
            score <- 0;
            match next with
            | Some mem  -> mem#reset m main
            | None      ->
                    main#reset m main;
                    next <- Some main

        method process start write addr size =
            let result = send_request self#next start write addr size in
            score <- score + result;
            result

        method finish =
            let result = self#next#finish in
            score <- score + result;
            result

    end
