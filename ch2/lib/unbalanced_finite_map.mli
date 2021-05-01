open Common

module Make (Key : Ordered_intf.S) : Finite_map_intf.S with type key = Key.t
