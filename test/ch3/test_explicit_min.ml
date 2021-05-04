open Base
open Stdio
open Okasaki_src

module H = Explicit_min.Make(Binomial_heap.Make(Int))

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

let%expect_test "test delete_min returns empty on singleton" =
  let h =
    H.empty
    |> H.insert ~value:5
  in

  H.delete_min h |> H.is_empty  |> printf "%b\n";

  [%expect {| true |}]
