open Core_bench
open Okasaki_src
open Base

module US = Unbalanced_set.Make(Int)
module RBS = Red_black_set.Make(Int)

(* let nums = List.init 1_000_000 ~f:(fun _ -> Random.int Int.max_value) *)
let nums = List.init 10_000 ~f:(fun n -> n)

let () =
  let f (module S : Set_intf.S with type elm = Int.t) (l : int list) =
    let h = l |> List.fold ~init:S.empty ~f:(fun h n -> S.insert h ~value:n) in
    l |> List.iter ~f:(fun n -> ignore @@ S.member h ~value:n)
  in
  [
    Bench.Test.create ~name:"Unbalanced set" (fun () -> f (module US) nums);
    Bench.Test.create ~name:"Red black set" (fun () -> f (module RBS) nums);
  ]
  |> Bench.bench ~display_config:(Bench.Display_config.create ~show_percentage:true ())
