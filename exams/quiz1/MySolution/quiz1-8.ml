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

let smallest_starting_from (t: int*int*int*int*int) (i:int): int*int*int*int*int = 
  let (a, b, c, d, e) = t in
  if i <= 0 && a < b && a < c && a < d && a < e 
    then t
  else if i <= 1 && b < c && b < d && b < e 
    then (b, a, c, d, e)
  else if i <= 2 && c < d && c < e 
    then (c, b, a, d, e)
  else if i <= 3 && d < e
    then (d, b, c, a, e)
  else (e, b, c, d, a)
;;

let sort5: int*int*int*int*int -> int*int*int*int*int =
  fun p0 -> 
    let p1 = smallest_starting_from p0 0 in
    let p2 = smallest_starting_from p1 1 in
    let p3 = smallest_starting_from p2 2 in
    let p4 = smallest_starting_from p3 3 in
    let p5 = smallest_starting_from p4 4 in
    p5
  ;;

  sort5(1, 2, 1, 1, 2);;
  sort5(1, 3, 4, 5, 2);;
  sort5(1, 3, 5, 4, 2);;

(* ************************************************ *)
