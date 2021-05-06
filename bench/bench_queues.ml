open Core_bench
open Okasaki_book
open Base

module BQ = Batched_queue

let () =
  let f (module Q : Queue_intf.S) =
    let rec loop n (q : int Q.t) =
      if n = 0 then ()
      else match Q.is_empty q, Random.bool () with
        | true, _
        | false, true -> Q.snoc q ~value:1 |> loop (n - 1)
        | false, false -> Q.tail q |> loop (n - 1)
    in
    loop 10_000_000 Q.empty
  in
  [
    Bench.Test.create ~name:"Batched queue" (fun () -> f (module BQ));
  ]
  |> Bench.bench ~display_config:(Bench.Display_config.create ~show_percentage:true ())
