open Base
open Stdio
open Okasaki_book

module T = Trie_2.Make(Assoc_list.Make(Char))

let%expect_test "test" =
  let l = [ ((String.to_list "cat"), 1);
            ((String.to_list "cart"), 3);
            ((String.to_list "dog", 4)); ] in
  let t = List.fold l ~init:T.empty ~f:(fun acc (key, value) -> T.bind acc ~key ~value) in

  List.iter l ~f:(fun (key, _) -> T.lookup t ~key |> printf "%d ");

  [%expect {| 1 3 4 |}]
