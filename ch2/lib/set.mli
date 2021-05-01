module type OrderedType = sig
  type t

  val compare : t -> t -> int
end

module type S = sig
  type elt
  type t

  val empty : t
  val insert : t -> value:elt -> t
  val member : t -> value:elt -> bool
  val complete : value:elt -> depth:int -> t
end

module Make (Element : OrderedType) : S with type elt = Element.t
