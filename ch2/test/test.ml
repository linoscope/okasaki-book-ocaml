open Base
open Stdio
open Ch2

let s = Custom_stack.(cons 1 (cons 2 (cons 3 empty)))

let print_int_list s =
  s
  |> Custom_stack.sexp_of_t Int.sexp_of_t
  |> Sexp.to_string_hum
  |> print_endline

let print_int_list_list s =
  s
  |> Custom_stack.sexp_of_t (Custom_stack.sexp_of_t Int.sexp_of_t)
  |> Sexp.to_string_hum
  |> print_endline

let%expect_test "list append" =
  let t = Custom_stack.(cons 4 (cons 5 empty)) in
  let actual = Custom_stack.(s ++ t) in
  actual |> print_int_list;
  [%expect {| (Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 Nil))))) |}]

let%expect_test "list update" =
  let actual = Custom_stack.update s 1 12 in
  actual |> print_int_list;
  [%expect {| (Cons 1 (Cons 12 (Cons 3 Nil))) |}]

let%expect_test "list suffixes" =
  let actual = Custom_stack.suffixes s in
  actual |> print_int_list_list;
  [%expect {|
    (Cons (Cons 1 (Cons 2 (Cons 3 Nil)))
     (Cons (Cons 2 (Cons 3 Nil)) (Cons (Cons 3 Nil) Nil))) |}]
