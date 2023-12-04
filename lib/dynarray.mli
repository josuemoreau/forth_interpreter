type 'a t

val create : int -> 'a -> 'a t
val get : 'a t -> int -> 'a
val set : 'a t -> int -> 'a -> unit
val push : 'a t -> 'a -> unit
val pop : 'a t -> 'a
val length : 'a t -> int
val iter : ('a -> unit) -> 'a t -> unit