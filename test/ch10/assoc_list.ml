open Okasaki_book

module Make (Key : Ordered_intf.S) = struct
  type key = Key.t

  type 'a t = (key * 'a) list

  let empty = []

  let rec lookup t ~key =
    match t with
    | [] -> raise Not_found
    | (k, v)::xs ->
      if Key.compare key k = 0 then
        v
      else
        lookup xs ~key

  let bind t ~key ~value = (key, value)::t
end
