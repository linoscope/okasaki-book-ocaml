module Make (Heap : Heap_intf.S) = struct
  exception Empty

  module Elm = Heap.Elm

  type t = E | NE of Elm.t * Heap.t

  let empty = E

  let is_empty = function E -> true | NE _ -> false

  let insert t ~value =
    match t with
    | E -> NE (value, Heap.insert Heap.empty ~value)
    | NE (me, h) ->
      if Elm.(value < me) then
        NE (value, Heap.insert h ~value)
      else
        NE (me, Heap.insert h ~value)

  let merge t1 t2 =
    match t1, t2 with
    | E, _ -> t2
    | _, E -> t1
    | NE (me1, h1), NE (me2, h2) ->
      if Elm.(me1 < me2) then
        NE (me1, Heap.merge h1 h2)
      else
        NE (me2, Heap.merge h1 h2)

  let find_min = function
    | E -> raise Empty
    | NE (me, _) -> me

  let delete_min = function
    | E -> raise Empty
    | NE (_, h) ->
      let h' = Heap.delete_min h in
      if Heap.is_empty h' then
        E
      else
        NE (Heap.find_min h', h')
end
