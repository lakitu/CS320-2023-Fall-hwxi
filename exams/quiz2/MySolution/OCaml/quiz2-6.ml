(* ************************************************ *)

(*
Q2-6: 10 points

The function list_reverse return the reverse of a given list.
Please give an implementation of list_reverse based on list_foldright
(not list_foldleft).
*)

(* ************************************************ *)

let list_reverse(xs: 'a list): 'a list = 
  list_foldright(xs)([])
  (fun x0 r0 ->
    list_foldright(r0)([x0])(fun ri xn -> ri :: xn)
  )
;;
