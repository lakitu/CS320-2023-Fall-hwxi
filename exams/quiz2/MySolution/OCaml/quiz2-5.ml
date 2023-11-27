(* ************************************************ *)

(*
Q2-5: 5 points
The function list_last returns the last element of a given
list. Please give a NON-RECURSIVE implementation of list_last
based on pattern matching and list_foldright. If the given list
is empty, raise the Empty exception
*)

(* ************************************************ *)

let list_reverse xs = list_foldright(xs)([])
  (fun x0 r0 ->
    list_foldright(r0)([x0])(fun ri xn -> ri :: xn)
  )
  ;;

exception Empty
let list_last(xs: 'a list): 'a = 
  match xs with
  | [] -> raise Empty
  | x0 :: xs -> list_foldright(list_reverse(xs))(x0)(fun x0 r0 -> x0)
;;
