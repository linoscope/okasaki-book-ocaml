module Make (Element : Ordered_intf.S) : sig
  include Set_intf.S with type elm = Element.t
  val complete : value:elm -> depth:int -> t
end
