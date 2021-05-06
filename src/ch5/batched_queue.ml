exception Empty

type 'a t = 'a list * 'a list

let empty = ([], [])

let is_empty = function
  | [], [] -> true
  | _::_, _ | _, _::_ -> false

let head = function
  | [], _ -> raise Empty
  | h::_, _ -> h

let check_f = function
  | ([], r) -> (List.rev r, [])
  | t -> t

let tail = function
  | [], _ -> raise Empty
  | _::f, r -> check_f (f, r)

let snoc (f, r) ~value = check_f (f, value::r)
