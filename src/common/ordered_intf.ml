module type S = sig
  type t

  val compare : t -> t -> int
  val ( = ) : t -> t -> bool
  val ( < ) : t -> t -> bool
  val ( <= ) : t -> t -> bool
end
