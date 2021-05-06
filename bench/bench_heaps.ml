open Core_bench
open Okasaki_book
open Base

module LH = Leftist_heap.Make(Int)
module WBLH = Weight_biased_leftist_heap.Make(Int)
module BH = Binomial_heap.Make(Int)

let () =
  let nums = List.init 1_000_000 ~f:(fun _ -> Random.int Int.max_value) in
  let insert_and_delete (module H : Heap_intf.S with type Elm.t = Int.t) (l : int list) =
    let rec del_loop h =
      if H.is_empty h then ()
      else del_loop (H.delete_min h)
    in
    let h = l |> List.fold ~init:H.empty ~f:(fun h n -> H.insert h ~value:n) in
    del_loop h
  in
  [
    Bench.Test.create ~name:"Lefitst Heap" (fun () -> insert_and_delete (module LH) nums);
    Bench.Test.create ~name:"Weight Biased Lefitst Heap" (fun () -> insert_and_delete (module WBLH) nums);
    Bench.Test.create ~name:"Binomial Heap" (fun () -> insert_and_delete (module BH) nums);
  ]
  |> Bench.bench ~display_config:(Bench.Display_config.create ~show_percentage:true ())
