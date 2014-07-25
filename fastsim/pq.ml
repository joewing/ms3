type 'a t = {
    mutable heap : (int * 'a option) array;
    mutable size : int
}

let create depth =
    {
        heap = Array.make (depth + 1) (0, None);
        size = 0
    }
;;

let swap (q : 'a t) (a : int) (b : int) : int =
    let temp = q.heap.(a) in
    q.heap.(a) <- q.heap.(b);
    q.heap.(b) <- temp;
    b
;;

let reset q =
    q.size <- 0
;;

let get_key q =
    fst q.heap.(1)
;;

let get_value q =
    match snd q.heap.(1) with
    | Some t -> t
    | None -> failwith "empty heap"
;;

let is_empty q =
    q.size = 0
;;

let push q key value =
    q.size <- q.size + 1;
    let len = Array.length q.heap in
    if q.size >= len then
        let extension = Array.make len (0, None) in
        q.heap <- Array.append q.heap extension
    else ();
    q.heap.(q.size) <- (key, Some value);
    let i = ref q.size in
    while !i > 1 do
        let ni = !i / 2 in
        if (fst q.heap.(!i)) < (fst q.heap.(ni)) then
            i := swap q !i ni
        else i := 0
    done
;;

let pop q =
    let result = get_value q in
    q.heap.(1) <- q.heap.(q.size);
    q.size <- q.size - 1;
    let i = ref 1 in
    while !i < q.size do
        let left = !i * 2 in
        let right = left + 1 in
        if right <= q.size then
            if (fst q.heap.(left)) < (fst q.heap.(right)) then
                if (fst q.heap.(!i) ) > (fst q.heap.(left)) then
                    i := swap q !i left
                else i := q.size
            else if (fst q.heap.(!i)) > (fst q.heap.(right)) then
                i := swap q !i right
            else i := q.size
        else if left <= q.size && (fst q.heap.(!i)) > (fst q.heap.(left)) then
            i := swap q !i left
        else i := q.size
    done; result
;;
