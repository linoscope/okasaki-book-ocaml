open Base
open Stdio
open Okasaki_book

module H = Weight_biased_leftist_heap.Make(Int)

let%expect_test "test" =
  let h =
    H.empty
    |> H.insert ~value:5
    |> H.insert ~value:2
    |> H.insert ~value:4
    |> H.insert ~value:3
  in

  h |> H.find_min |> printf "%d\n";
  H.delete_min h |> H.find_min |> printf "%d\n";
  H.delete_min (H.delete_min h) |> H.find_min |> printf "%d\n";
  H.delete_min (H.delete_min (H.delete_min h)) |> H.find_min |> printf "%d\n";

  [%expect {|
    2
    3
    4
    5 |}]
