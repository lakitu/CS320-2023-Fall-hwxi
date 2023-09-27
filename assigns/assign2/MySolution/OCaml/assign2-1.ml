(* #use "../../../../classlib/OCaml/MyOCaml.ml";; *)
#use "../../assign2.ml";;

let rec mylist_length(xs: 'a mylist): int =
  match xs with
  | MyNil -> 0
  | MyCons(xi, xs) -> 1 + mylist_length(xs)
  | MySnoc(xs, xi) -> 1 + mylist_length(xs)
  | MyAppend2(xs1, xs2) -> mylist_length(xs1) + mylist_length(xs2)
  | MyReverse(xs) -> mylist_length(xs)
;;