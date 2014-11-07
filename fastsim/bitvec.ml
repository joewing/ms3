
type t = bool array;;

let make n = Array.make n false;;

let intersect a b =
    Array.iteri (fun i v ->
        a.(i) <- a.(i) && v
    ) b
;;

let clear a = Array.fill a 0 (Array.length a) false;;

let get a i = a.(i);;

let set a i v = a.(i) <- v;;

let is_empty a =
    let alen = Array.length a in
    let rec helper i =
        if i = alen then true
        else if a.(i) then false
        else helper (i + 1)
    in helper 0
;;

let choose a =
    let rec helper i =
        if a.(i) then i
        else helper (i + 1)
    in helper 0
;;

let exists f a =
    let alen = Array.length a in
    let rec helper i =
        if i = alen then false
        else if a.(i) && f i then true
        else helper (i + 1)
    in helper 0
;;

let update a f =
    let alen = Array.length a in
    let rec helper i =
        if i = alen then ()
        else if a.(i) then a.(i) <- f i
        else ()
    in helper 0
;;

let copy a = Array.copy a;;
