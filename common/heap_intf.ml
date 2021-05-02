module type S = sig
  exception Empty

  module Elm : Ordered_intf.S
  type t

  val empty : t
  val is_empty : t -> bool

  val insert : t -> value:Elm.t -> t
  val merge : t -> t -> t

  val find_min : t -> Elm.t
  val delete_min : t -> t

  val of_list : Elm.t list -> t
end
