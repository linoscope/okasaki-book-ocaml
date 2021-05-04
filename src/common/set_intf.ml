module type S = sig
  type elm
  type t

  val empty : t
  val insert : t -> value:elm -> t
  val member : t -> value:elm -> bool
end
