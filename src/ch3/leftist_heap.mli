module Make (Element : Ordered_intf.S) : sig
  include Heap_intf.S with module Elm = Element
  val of_list : Elm.t list -> t
end
