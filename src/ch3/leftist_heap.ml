open Okasaki_common
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
      T (rank r, v, l, r)
    else
      T (rank l, v, r, l)

  let rec merge t1 t2 =
    match t1, t2 with
    | E, t2 -> t2
    | t1, E -> t1
    | T (_, v1, l1, r1), T (_, v2, l2, r2) ->
      match Elm.compare v1 v2 with
      | -1
      |  0 -> make_t (v1, l1, merge r1 t2)
      |  1 -> make_t (v2, l2, merge t1 r2)
      |  _ -> assert false

  let insert t ~value = merge t (T (0, value, E, E))

  let find_min = function
    | E -> raise Empty
    | T (_, v, _, _) -> v

  let delete_min = function
    | E -> raise Empty
    | T (_, _, l, r) -> merge l r
end
