open Base

module Make (Element : Ordered_intf.S) = struct
  exception Empty

  module Elm = Element

  type t = E | T of Elm.t * t list

  let empty = E

  let is_empty = function
    | E -> true
    | T (_, _) -> false

  let merge t1 t2 =
    match t1, t2 with
    | E, E -> E
    | E, _ -> t2
    | _, E -> t1
    | T (x1, ts1), T (x2, ts2) ->
      if Elm.(x1 < x2) then
        T (x1, t2::ts1)
      else
        T (x2, t1::ts2)

  let insert t ~value = merge t (T (value, []))

  let find_min = function
    | E -> raise Empty
    | T (x, _) -> x

  (* let rec merge_pairs : t list -> t = function
   *   | [] -> E
   *   | [t] -> t
   *   | t1::t2::ts ->
   *     merge (merge t1 t2) (merge_pairs ts) *)

  (* The above commmented out implementation from the book causes stack overflow
   * when run with 10^6 elements (noticed this when trying to run bench_heaps.ml.).
   * We split the process into two tail recursive functions to avoid this.
   *  *)
  let merge_pairs ts =
    let merge_pairs ts =
      let rec loop acc = function
        | [] -> acc
        | [t] -> t::acc
        | t1::t2::ts -> loop ((merge t1 t2)::acc) ts
      in
      loop [] ts
    in
    let merge_list = List.fold_left ~init:E ~f:merge
    in
    ts |> merge_pairs |> merge_list
  ;;
  let delete_min = function
    | E -> raise Empty
    | T (_, ts) -> merge_pairs ts
end
