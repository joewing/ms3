open Base_memory

class subsystem =
    object (self)
        inherit base_memory as super

        val mutable index : int = 0
        val mutable word_size : int = 4
        val mutable size : int = 16777216
        val mutable mem : base_memory option = None
        val mutable offset : int = 0
        val mutable score : int = 0

        method get_id = index

        method set name value = match name with
        | "id" -> index <- int_of_string value
        | "size" -> size <- int_of_string value
        | "word_size" -> word_size <- int_of_string value
        | _ -> super#set name value

        method get_word_size = word_size

        method set_offset o = offset <- o

        method total_size = size

        method get_next = match mem with
            | Some m -> m
            | None -> failwith "no next"

        method set_next n = mem <- n

        method reset m main =
            super#reset m main;
            score <- 0;
            match mem with
            | Some mem  -> mem#reset m main
            | None      ->
                    main#reset m main;
                    mem <- Some main

        method process start write addr size =
            let result = send_request self#get_next start write addr size in
            score <- score + result;
            result

        method finish =
            let result = self#get_next#finish in
            score <- score + result;
            result

    end
