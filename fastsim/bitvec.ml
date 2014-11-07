
type t = bool array;;

(* Create a bit vector of length n. *)
let make n = Array.make n false;;

(* Clear all bits of a. *)
let clear a = Array.fill a 0 (Array.length a) false;;

(* Get bit i of a. *)
let get a i = a.(i);;

(* Set a bit in a to either true or false. *)
let set a i v = a.(i) <- v;;

(* Return true if no bits are set in a. *)
let is_empty a =
    let alen = Array.length a in
    let rec helper i =
        if i = alen then true
        else if a.(i) then false
        else helper (i + 1)
    in helper 0
;;

(* Return the index of the first set bit. *)
let first a =
    let rec helper i =
        if a.(i) then i
        else helper (i + 1)
    in helper 0
;;

(* Check if there exists a bit in a that is true for which f is
 * also true. *)
let exists f a =
    let alen = Array.length a in
    let rec helper i =
        if i = alen then false
        else if a.(i) && f i then true
        else helper (i + 1)
    in helper 0
;;

(* Update a with the intersection of a and the result of f. *)
let update a f =
    let alen = Array.length a in
    let rec helper i =
        if i = alen then ()
        else
            begin
                a.(i) <- a.(i) && f i;
                helper (i + 1)
            end
    in helper 0
;;

(* Copy the contents of b to a. *)
let copy a b =
    let alen = Array.length a in
    Array.blit b 0 a 0 alen
;;
