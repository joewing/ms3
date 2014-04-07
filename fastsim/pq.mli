type 'a t

val create : int -> 'a t

val reset : 'a t -> unit

val get_key : 'a t -> int

val get_value : 'a t -> 'a

val is_empty : 'a t -> bool

val push : 'a t -> int -> 'a -> unit

val pop : 'a t -> 'a
