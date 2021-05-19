module Make
    (Approx : Finite_map_intf.S)
    (Exact : Finite_map_intf.S)
    (HashF : sig val hash : Exact.key -> Approx.key end) : Finite_map_intf.S with type key = Exact.key
