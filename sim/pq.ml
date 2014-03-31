
class ['a] pq =
    object (self)

        val heap : (int * 'a option) array = Array.make 256 (0, None)
        val mutable size : int = 0

        method private swap a b =
            let temp = heap.(a) in
            heap.(a) <- heap.(b);
            heap.(b) <- temp;
            b

        method reset = size <- 0

        method key = fst heap.(1)

        method value =
            match snd heap.(1) with
            | Some a -> a
            | None -> failwith "empty heap"

        method empty = size = 0

        method push key value =
            size <- size + 1;
            heap.(size) <- (key, Some value);
            let i = ref size in
            while !i > 1 do
                let ni = !i / 2 in
                if (fst heap.(!i)) < (fst heap.(ni)) then
                    i := self#swap !i ni
                else i := 0
            done

        method pop () =
            heap.(1) <- heap.(size);
            size <- size - 1;
            let i = ref 1 in
            while !i < size do
                let left = !i * 2 in
                let right = left + 1 in
                if right <= size then
                    if (fst heap.(left)) < (fst heap.(right)) then
                        if (fst heap.(!i) ) > (fst heap.(left)) then
                            i := self#swap !i left
                        else i := size
                    else if (fst heap.(!i)) > (fst heap.(right)) then
                        i := self#swap !i right
                    else i := size
                else if left <= size && (fst heap.(!i)) > (fst heap.(left)) then
                    i := self#swap !i left
                else i := size
            done

    end
