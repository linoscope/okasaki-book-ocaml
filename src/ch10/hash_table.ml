module Make
    (Approx : Finite_map_intf.S)
    (Exact : Finite_map_intf.S)
    (HashF : sig val hash : Exact.key -> Approx.key end) =
struct
  open HashF

  type key = Exact.key

  type 'a t = 'a Exact.t Approx.t

  let empty = Approx.empty

  let lookup t ~key =
    Approx.lookup t ~key:(hash key) |> Exact.lookup ~key:key

  let bind t ~key ~value =
    let h = hash key in
    let exact = try Approx.lookup t ~key:h with Not_found -> Exact.empty in
    Approx.bind t ~key:h ~value:(Exact.bind exact ~key ~value)
end
