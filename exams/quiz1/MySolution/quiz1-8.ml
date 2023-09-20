(* ************************************************ *)

(*

 Question 8: 20 points
 Please give a NON-RECURSIVE implementation of sort5
 that takes 5 integers and returns a tuple that consists
 exactly of the 5 given integers ordered increasingly

 For instance, sort5(1, 2, 1, 2, 1) = (1, 1, 1, 2, 2)
 For instance, sort5(1, 3, 4, 5, 2) = (1, 2, 3, 4, 5)
 For instance, sort5(1, 3, 5, 4, 2) = (1, 2, 3, 4, 5)

 You can implement your own helper functions as long as
 you do not make use of recursion.

*)


let sort2 (t:int*int*int*int*int):int*int*int*int*int =
  let (a, b, c, d, e) = t in
  if b < a then (b, a, c, d, e)
  else t
;;

let sort3 (t:int*int*int*int*int):int*int*int*int*int =
  let (a, b, c, d, e) = t in
  if c < b then sort2 (a, c, b, d, e)
  else sort2(t)
;;

let sort4 (t: int*int*int*int*int): int*int*int*int*int =
  let (a, b, c, d, e) = t in
  if d < c then sort3(a, b, d, c, e)
  else sort3(t)
;;

let sort5(t: int*int*int*int*int): int*int*int*int*int =
  let (a, b, c, d, e) = t |> sort2 |> sort3 |> sort4 in
  if e < d then sort4(a, b, c, e, d)
  else sort4(a, b, c, d, e)
;;

  sort5(1, 2, 1, 1, 2);;
  sort5(1, 3, 4, 5, 2);;
  sort5(1, 3, 5, 4, 2);;
  sort5(5, 4, 3, 2, 1);;

(* ************************************************ *)
