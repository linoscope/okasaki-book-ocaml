open Base
open Stdio
open Okasaki_book

let rec from n = lazy (Stream.Cons (n, from (n + 1)))
let nats = from 0

let print_stream s =
  s
  |> Stream.to_list
  |> List.sexp_of_t Int.sexp_of_t
  |> Sexp.to_string
  |> print_endline


let%expect_test "take" =
  let s = nats |> Stream.take ~len:10 in

  s |> print_stream;

  [%expect {| (0 1 2 3 4 5 6 7 8 9) |}]

let%expect_test "drop" =
  let s = nats |> Stream.drop ~len:10 in

  s |> Stream.take ~len:10 |> print_stream;

  [%expect {| (10 11 12 13 14 15 16 17 18 19) |}]

let%expect_test "rev" =
  let s = nats |> Stream.take ~len:10 |> Stream.rev in

  s |> print_stream;

  [%expect {| (9 8 7 6 5 4 3 2 1 0) |}]
