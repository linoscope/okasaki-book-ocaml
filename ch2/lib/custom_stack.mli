  type 'a t [@@deriving sexp_of]

  val empty : 'a t

  val is_empty : 'a t -> bool

  val cons : 'a -> 'a t -> 'a t

  val head : 'a t -> 'a

  val ( ++ ): 'a t -> 'a t -> 'a t

  val update: 'a t -> int -> 'a -> 'a t

  val suffixes: 'a t -> 'a t t
