open Base
open Stdio
open Okasaki_book

module M =
  Hash_table.Make
    (Assoc_list.Make (Int))
    (Assoc_list.Make (String))
    (struct let hash = String.hash end)


let%expect_test "test" =
  let m =
    M.empty
    |> M.bind ~key:"foo" ~value:'a'
    |> M.bind ~key:"bar" ~value:'b'
    |> M.bind ~key:"baz" ~value:'c'
  in

  m |> M.lookup ~key:"foo" |> Char.to_string |> print_endline;
  m |> M.lookup ~key:"bar" |> Char.to_string |> print_endline;
  m |> M.lookup ~key:"baz" |> Char.to_string |> print_endline;

  [%expect {|
    a
    b
    c |}]
