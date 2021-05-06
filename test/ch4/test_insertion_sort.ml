open Base
open Stdio
open Okasaki_src

let print_stream s =
  s
  |> Stream.to_list
  |> List.sexp_of_t Int.sexp_of_t
  |> Sexp.to_string
  |> print_endline

let%expect_test "sort" =
  let s = Stream.of_list [8; 2; 4; 1; 3; 7; 9; 6; 5] |> Insertion_sort.f in

  s |> print_stream;

  [%expect {| (1 2 3 4 5 6 7 8 9) |}]
