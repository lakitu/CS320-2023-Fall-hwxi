(* 
   My crazed and frantic writings post-quiz. 
   Basically recording my lightbulb moments before I could write it down in this repo.
   Also, Jason is the best TA!
*)

#use "../classlib/OCaml/MyOCaml.ml";;


let list_reverse xs = 
  list_foldright(xs)([])(fun x0 r0 ->
    list_foldright(r0)([x0])(fun ri xn -> ri :: xn)
  )
;;

let list_get_last xs = match xs with [] -> raise Empty | x0 :: xs -> list_foldright(list_reverse(xs))(x0)(fun x0 r0 -> x0);;