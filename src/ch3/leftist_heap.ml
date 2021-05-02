open Base

module Make (Element : Ordered_intf.S) = struct
  exception Empty

  module Elm = Element

  type t = E | T of int * Elm.t * t  * t

  let empty = E

  let is_empty = function
    | E -> true
    | T _ -> false

  let rank = function
    | E -> 0
    | T (k, _, _, _) -> k

  let make_t (v, l, r) =
    if rank l >= rank r then
      T (rank r + 1, v, l, r)
    else
      T (rank l + 1, v, r, l)

  let rec merge t1 t2 =
    match t1, t2 with
    | E, t2 -> t2
    | t1, E -> t1
    | T (_, v1, l1, r1), T (_, v2, l2, r2) ->
      if Elm.(v1 <= v2) then
        make_t (v1, l1, merge r1 t2)
      else
        make_t (v2, l2, merge t1 r2)

  (* let insert t ~value = merge t (T (0, value, E, E)) *)

  let rec insert t ~value =
    match t with
    | E -> T (1, value, E, E)
    | T (_, v, l, r) ->
      if Elm.(value <= v) then
        T (1, value, t, E)
      else
        make_t (v, l, insert r ~value)

  let find_min = function
    | E -> raise Empty
    | T (_, v, _, _) -> v

  let delete_min = function
    | E -> raise Empty
    | T (_, _, l, r) -> merge l r

  let of_list l =
    let elm_to_node v = T (1, v, E, E) in
    let rec merge_nodes (ns : t list) (acc : t list) : t =
      match ns, acc with
      | [], [] -> E
      | [], [x] -> x
      | [], acc -> merge_nodes acc []
      | [x], acc -> merge_nodes [] (x::acc)
      | x::y::xs, acc -> merge_nodes xs ((merge x y)::acc)
    in
    l
    |> List.map ~f:elm_to_node
    |> (fun ns -> merge_nodes ns [])
end
