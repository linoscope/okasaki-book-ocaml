module Make (M : Finite_map_intf.S) = struct
  type key = M.key list

  type 'a t = Entry of 'a | Node of 'a t M.t

  let empty = Node M.empty

  let rec lookup t ~key =
    match key, t with
    | [], Node _
    | _::_, Entry _ -> raise Not_found
    | [], Entry x -> x
    | x::xs, Node m ->
      let t' = M.lookup m ~key:x in
      lookup t' ~key:xs

  let rec bind t ~key ~value =
    match key, t with
    | [], _ -> Entry value
    | x::xs, Entry _ -> Node (M.bind M.empty ~key:x ~value:(bind empty ~key:xs ~value))
    | x::xs, Node m ->
      let t' = try M.lookup m ~key:x with Not_found -> empty in
      Node (M.bind m ~key:x ~value:(bind t' ~key:xs ~value))
end
