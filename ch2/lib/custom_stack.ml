open Base


type 'a t = Nil | Cons of 'a * 'a t [@@deriving sexp_of]

let empty = Nil

let is_empty = function
  | Nil -> true
  | Cons _ -> false

let cons h t = Cons(h, t)

let head = function
  | Nil -> failwith "Stack is empty"
  | Cons(x, _) -> x

let rec ( ++ ) t1 t2 = match t1 with
  | Nil -> t2
  | Cons(x, xs) -> Cons(x, xs ++ t2)

let rec update t i a = match (t, i) with
  | Nil, _ -> failwith "subscript"
  | Cons (_, xs), 0 -> Cons (a, xs)
  | Cons (x, xs), i -> Cons (x, update xs (i - 1) a)

let rec suffixes = function
  | Nil -> Nil
  | Cons (_, xs) as t -> Cons(t, suffixes xs)
