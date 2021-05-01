open Base
open Stdio
open Ch2

module M = Unbalanced_finite_map.Make(Int)

let%expect_test "lookup" =
  let fm =
    M.empty
    |> M.bind ~key:1 ~value:'a'
    |> M.bind ~key:3 ~value:'c'
    |> M.bind ~key:2 ~value:'b'
  in
  fm |> M.lookup ~key:1 |> Char.to_string |> print_endline;
  fm |> M.lookup ~key:2 |> Char.to_string |> print_endline;
  fm |> M.lookup ~key:3 |> Char.to_string |> print_endline;
  [%expect {|
    a
    b
    c |}]
