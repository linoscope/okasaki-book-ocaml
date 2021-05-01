open Okasaki_common

module Make (Element : Ordered_intf.S) : Set_intf.S with type elt = Element.t
