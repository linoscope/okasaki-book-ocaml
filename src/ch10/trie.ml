module Make (M : Finite_map_intf.S) = struct
  type key = M.key list

  type 'a t = Node of 'a option * 'a t M.t

  let empty = Node (None, M.empty)

  let rec lookup t ~key =
    match key, t with
    | [], Node (None, _) -> raise Not_found
    | [], Node (Some x, _) -> x
    | x::xs, Node (_, m) ->
      let t' = M.lookup m ~key:x in
      lookup t' ~key:xs

  let rec bind t ~key ~value =
    match key, t with
    | [], Node (_, m) -> Node (Some value, m)
    | x::xs, Node (v, m) ->
      let t' = try M.lookup m ~key:x with Not_found -> empty in
      Node (v, M.bind m ~key:x ~value:(bind t' ~key:xs ~value))
end
