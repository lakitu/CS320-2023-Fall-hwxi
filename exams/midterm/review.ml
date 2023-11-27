#use "../../classlib/OCaml/MyOCaml.ml";;

let foreach_to_forall(foreach: ('xs, 'x0) foreach): ('xs, 'x0) forall =
  fun (xs: 'xs)(test: 'x0 -> bool) ->
    let exception False in
    try (foreach(xs)(fun x0 -> if test(x0) = false then raise False); true)
    with False -> false
  ;;

(* exception IndexOutOfBounds;;
let list_get_at(xs: 'a list)(i: int) =
let exception (Found of 'a) in
try
  list_foldleft(xs)(0)
  (fun j xi -> (if i = j then raise(Found xi)); j + 1);
  raise IndexOutOfBounds
with Found(xi) -> xi
;; *)

let
foreach_to_head
(foreach: ('xs, 'x0) foreach): 'xs -> 'x0 =
fun xs ->
let
exception Found of 'x0 in
try
foreach(xs)
(fun x -> raise Found(x)); raise Empty
with Found(x) -> x
;;