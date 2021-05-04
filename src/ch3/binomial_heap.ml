open Base

module Make (Element : Ordered_intf.S) = struct
  exception Empty

  module Elm = Element

  type tree = Node of int * Elm.t * tree list

  type t = tree list

  let empty = []

  let is_empty = function [] -> true | _::_ -> false

  let link (Node (r, x1, c1) as tr1) (Node (_, x2, c2) as tr2) =
    if Elm.(x1 <= x2) then
      Node (r + 1, x1, tr2::c1)
    else
      Node (r + 1, x2, tr1::c2)

  let rank (Node (r, _, _)) = r

  let root (Node (_, x, _)) = x

  let rec insert_tree (tr : tree) : t -> t = function
    | [] -> [tr]
    | tr'::ts as t ->
      match Int.compare (rank tr) (rank tr') with
      | -1 -> tr::t
      |  0 -> insert_tree (link tr tr') ts
      |  _ -> assert false

  let insert t ~value = insert_tree (Node (0, value, [])) t

  let rec merge t1 t2 =
    match t1, t2 with
    | [], _ -> t2
    | _, [] -> t1
    | tr1::ts1, tr2::ts2 ->
      match Int.compare (rank tr1) (rank tr2) with
      | -1 -> tr1::(merge ts1 t2)
      |  1 -> tr2::(merge t1 ts2)
      |  0 -> insert_tree (link tr1 tr2) (merge ts1 ts2)
      |  _ -> assert false

  let rec remove_min_tree (t : t) : tree * t =
    match t with
    | [] -> raise Empty
    | [tr] -> (tr, [])
    | tr::ts ->
      let (min_tr, remaining_t) = remove_min_tree ts in
      if Elm.(root tr < root min_tr) then
        (tr, ts)
      else
        (min_tr, tr::remaining_t)

  let find_min t =
    let (min_tr, _) = remove_min_tree t in
    root min_tr

  let delete_min t =
    let (min_tr, remaining_t) = remove_min_tree t in
    let Node (_, _, c) = min_tr in
    merge (List.rev c) remaining_t
end
