open Base
open Stdio
open Okasaki_book

module M = Red_black_set.Make(Int)

let create_test_set () =
  let open M in
  empty
  |> insert ~value:1
  |> insert ~value:3
  |> insert ~value:2

let%expect_test "member" =
  let s = create_test_set () in
  s |> M.member ~value:3 |> Bool.to_string |> print_endline;
  [%expect {| true |}]

let%expect_test "insert" =
  let s = create_test_set () in
  s |> M.insert ~value:4 |> M.member ~value:4 |> Bool.to_string |> print_endline;
  [%expect {| true |}]
