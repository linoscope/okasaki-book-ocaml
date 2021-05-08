open Base
open Stdio
open Okasaki_book

module DQ = Dequeue

let%expect_test "test head and tail" =
  let q = DQ.(empty |> cons ~value:2 |> cons ~value:1 |> snoc ~value:3) in

  q |> DQ.head |> printf "%d ";
  q |> DQ.tail |> DQ.head |> printf "%d ";
  q |> DQ.tail |> DQ.tail |> DQ.head |> printf "%d\n";

  [%expect {| 1 2 3 |}]

let%expect_test "test last and init" =
  let q = DQ.(empty |> snoc ~value:2 |> snoc ~value:1 |> cons ~value:3) in

  q |> DQ.last |> printf "%d ";
  q |> DQ.init |> DQ.last |> printf "%d ";
  q |> DQ.init |> DQ.init |> DQ.last |> printf "%d\n";

  [%expect {| 1 2 3 |}]
