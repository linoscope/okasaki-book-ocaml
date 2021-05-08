open Base

exception Empty

type 'a t = 'a list * 'a list

let empty = ([], [])

let is_empty = function
  | ([], []) -> true
  | (_::_, _) | (_, _::_) -> false

let split l =
  let rec loop n l acc =
    match n, l with
    | 0, _
    | _, [] -> (List.rev acc, List.rev l)
    | n, x::xs -> loop (n - 1) xs (x::acc)
  in
  loop (List.length l / 2) l []

let check t =
  match t with
  | ([], [_])
  | ([_], []) -> t
  | ([], r) -> let (a, b) = split r in (b, a)
  | (f, []) -> let (a, b) = split f in (a, b)
  | (_::_, _::_) -> t


let cons t ~value =
  match t with
  | ([x], []) -> ([value], [x])
  | (f, r) -> (value::f, r)

let head = function
  | [], [x] -> x
  | [], _ -> raise Empty
  | x::_, _ -> x

let tail = function
  | [], [_] -> empty
  | [], _ -> raise Empty
  | _::f, r -> check (f, r)

let snoc t ~value =
  match t with
  | ([], [x]) -> ([x], [value])
  | (f, r) -> (f, value::r)

let last = function
  | [x], [] -> x
  | _, [] -> raise Empty
  | _, x::_ -> x

let init = function
  | [_], [] -> empty
  | _, [] -> raise Empty
  | f, _::r -> check (f, r)
