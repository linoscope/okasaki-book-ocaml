open Base

type 'a cell = Nil | Cons of 'a * 'a t
and 'a t = 'a cell lazy_t

let rec ( ++ ) t1 t2 = lazy (
  match t1 with
  | lazy Nil -> Lazy.force t2
  | lazy (Cons (h1, s1)) -> Cons (h1, s1 ++ t2))

let rec take t ~len = lazy (
  match t, len with
  | _, 0 -> Nil
  | lazy Nil, _ -> Nil
  | lazy (Cons (h, s)), _ -> Cons (h, take s ~len:(len - 1)))

(* let rec drop t ~len = lazy (
 *   match t, len with
 *   | _, 0 -> Lazy.force t
 *   | lazy Nil, _ -> Nil
 *   | lazy (Cons (_, s)), _ -> Lazy.force (drop s ~len:(len - 1))) *)

let drop t ~len =
  let rec aux t n = match (t, n) with
    | _, 0 -> t
    | lazy Nil, _ -> lazy Nil
    | lazy (Cons (_, s)), _ -> aux s (n - 1)
  in
  aux t len

let rev t =
  let rec aux acc = function
    | lazy Nil -> acc
    | lazy (Cons (h, s)) -> aux (lazy (Cons (h, acc))) s
  in
  aux (lazy Nil) t

let to_list t =
  let rec aux acc = function
    | lazy Nil -> acc
    | lazy (Cons (h, s)) -> aux (h::acc) s
  in
  aux [] t |> List.rev

let rec of_list l = lazy (
  match l with
  | [] -> Nil
  | x::xs -> Cons (x, of_list xs))
