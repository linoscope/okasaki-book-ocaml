
type 'a cell = Nil | Cons of 'a * 'a t
and 'a t = 'a cell lazy_t

val ( ++ ) : 'a t -> 'a t -> 'a t

val take : 'a t -> len:int -> 'a t

val drop : 'a t -> len:int -> 'a t

val rev : 'a t -> 'a t

val to_list : 'a t -> 'a list

val of_list : 'a list -> 'a t
