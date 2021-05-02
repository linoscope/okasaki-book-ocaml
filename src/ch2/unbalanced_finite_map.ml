module Make (Key : Ordered_intf.S) = struct
  type key = Key.t
  type 'a t = E | T of 'a t * key * 'a * 'a t

  let empty = E

  let rec bind t ~key ~value =
    match t with
    | E -> T (E, key, value, E)
    | T (l, k, v, r) ->
      match Key.compare key k with
      |  0 -> T (l, k, value, r)
      | -1 -> T (bind l ~key ~value, k, v, r)
      |  1 -> T (l, k, v, bind r ~key ~value)
      |  _ -> assert false

  let rec lookup t ~key =
    match t with
    | E -> raise Not_found
    | T (l, k, v, r) ->
      match Key.compare key k with
      |  0 -> v
      | -1 -> lookup l ~key
      |  1 -> lookup r ~key
      |  _ -> assert false
end
