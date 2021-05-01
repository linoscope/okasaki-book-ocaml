module type S = sig
  type elt
  type t

  val empty : t
  val insert : t -> value:elt -> t
  val member : t -> value:elt -> bool
  val complete : value:elt -> depth:int -> t
end
