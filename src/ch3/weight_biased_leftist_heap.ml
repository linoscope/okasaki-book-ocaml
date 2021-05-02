open Base

module Make (Element : Ordered_intf.S) = struct
  exception Empty

  module Elm = Element

  type t = E | T of int * Elm.t * t * t

  let empty = E

  let is_empty = function
    | E   -> true
    | T _ -> false

  let weight = function
    | E -> 0
    | T (w, _, _, _) -> w

  (* let make_t w v l r =
   *   if weight l <= weight r then
   *     T (w, v, r, l)
   *   else
   *     T (w, v, l, r)
   *
   * let rec merge t1 t2 =
   *   match t1, t2 with
   *   | E, _ -> t2
   *   | _, E -> t1
   *   | T (w1, v1, l1, r1), T (w2, v2, l2, r2) ->
   *     if Elm.(v1 <= v2) then
   *       make_t (w1 + w2) v1 l1 (merge r1 t2)
   *     else
   *       make_t (w1 + w2) v2 l2 (merge r2 t1) *)

  let rec merge t1 t2 =
    match t1, t2 with
    | E, _ -> t2
    | _, E -> t1
    | T (w1, v1, l1, r1), T (w2, v2, l2, r2) ->
      if Elm.(v1 <= v2) then
        if weight l1 <= w2 + weight r1 then
          T (w1 + w2, v1, merge r1 t2, l1)
        else
          T (w1 + w2, v1, l1, merge r1 t2)
      else
      if weight l2 <= w1 + weight r2 then
        T (w1 + w2, v2, merge t1 r2, l2)
      else
        T (w1 + w2, v2, l2, merge t1 r2)

  let insert t ~value = merge t (T (1, value, E, E))

  let find_min = function
    | E -> raise Empty
    | T (_, v, _, _) -> v

  let delete_min = function
    | E -> raise Empty
    | T (_, _, r, l) -> merge l r

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
