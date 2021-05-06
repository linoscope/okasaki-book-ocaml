let rec f (s : 'a Stream.t) : 'a Stream.t =
  let open Stream in
  let rec insert x s = lazy (
    match s with
    | lazy Nil -> Cons (x, lazy Nil)
    | lazy (Cons (h, t)) ->
      if x <= h then Cons (x, s)
      else Cons (h, insert x t))
  in
  lazy (
    match s with
    | lazy Nil -> Nil
    | lazy (Cons (h, s)) -> Lazy.force @@ insert h (f s))
