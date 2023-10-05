(* ************************************************ *)

(*
Q2-7: 10 points

The following implementation of list_append is not tail-recursive.
Please give an implementation of list_append that is tail-recursive.

Note that you can only use pattern matching and list_foldleft in your
implementation.
 
let rec
list_append(xs: 'a list)(ys: 'a list) =
match xs with
  [] -> ys | x1 :: xs -> x1 :: list_append(xs)(ys)
*)

(* ************************************************ *)

let list_reverse (xs: 'a list): 'a list =
  list_foldleft(xs)([])(fun r0 x0 -> x0 :: r0)
;;

(* Implementation I wrote on the test (tail recursive loop) *)
let list_append(xs: 'a list)(ys: 'a list): 'a list = 
  let rec loop (xs)(ys)(acc) = 
    match xs with
    | [] -> (match ys with [] -> acc | y1 :: ys -> loop(xs)(ys)(y1 :: acc))
    | x1 :: xs -> loop(xs)(ys)(x1 :: acc)
  in list_reverse(loop(xs)(ys)([]))
;;

(* Implementation I wrote and erased on the test. Heavier on fold_left, less recursion *)
let list_append2(xs: 'a list)(ys: 'a list): 'a list = 
  list_foldleft
    (list_reverse(xs))
    (ys)
    (fun r0 x0 -> x0 :: r0)
;;
