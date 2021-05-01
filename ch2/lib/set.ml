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

module Make (Element : OrderedType) = struct
  type elt = Element.t
  type t = E | T of t * elt * t

  let empty = E

  let rec insert t ~value:x =
    match t with
    | E -> T (E, x, E)
    | T (l, y, r) ->
      match Element.compare x y with
      |  0 -> T (l, x, r)
      | -1 -> T (insert l ~value:x, y, r)
      |  1 -> T (l, y, insert r ~value:x)
      |  _ -> assert false

  let rec member t ~value:x =
    match t with
    | E -> false
    | T (l, y, r) ->
      match Element.compare x y with
      |  0 -> true
      | -1 -> member l ~value:x
      |  1 -> member r ~value:x
      |  _ -> false

  let rec complete ~value:x ~depth:d =
    match d with
    | 0 -> empty
    | d when d < 0 -> raise (Invalid_argument "negative argument")
    | d ->
      let subtree = complete ~value:x ~depth:(d - 1) in
      T (subtree, x, subtree)
end
