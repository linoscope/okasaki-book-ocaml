module Make (Element : Ordered_intf.S) = struct
  type elm = Element.t

  type color = R | B

  type t = E | T of color * elm * t * t

  let empty = E

  let rec member t ~value =
    match t with
    | E -> false
    | T (_, x, l, r) ->
      match Element.compare value x with
      |  0 -> true
      | -1 -> member l ~value
      |  1 -> member r ~value
      |  _ -> assert false

  let balance = function
    | (B, z, T (R, y, T (R, x, a, b), c), d)
    | (B, z, T (R, x, a, T (R, y, b, c)), d)
    | (B, x, a, T (R, y, T (R, z, b, c), d))
    | (B, x, a, T (R, y, b, T (R, z, c, d))) -> T (R, y, T (B, x, a, b), T (B, z, c, d))
    | (c, v, l, r) -> T (c, v, l, r)

  let insert t ~value =
    let rec ins = function
      | E -> T (R, value, E, E)
      | T (c, x, l, r) ->
        match Element.compare value x with
        |  0 -> T (c, x, l, r)
        | -1 -> balance (c, x, ins l, r)
        |  1 -> balance (c, x, l, ins r)
        |  _ -> assert false
    in
    match ins t with
    | E -> assert false
    | T (_, x, l, r) -> T (B, x, l, r)
end
