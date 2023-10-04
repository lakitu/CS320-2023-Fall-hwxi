#use "../../assign2.ml";;
#use "../../../../classlib/ocaml/myocaml.ml"

(*
Please implement foldleft_to_iforeach that turns a
foldleft-loop into a iforeach-loop:
let foldleft_to_iforeach
(foldleft: ('xs, 'x0, int) foldleft): ('xs, 'x0) iforeach = ...
*)

(* let rec
list_foldleft
(xs: 'a list)
(r0: 'r0)(fopr: 'r0 -> 'a -> 'r0): 'r0 =
match xs with
| [] -> r0
| (x1 :: xs) ->
  list_foldleft(xs)(fopr(r0)(x1))(fopr)
;; *)
  

let len(list: 'a list): int =
  list_foldleft(list)(0)(fun r0 x0 -> r0 + 1)
;;

let foldleft_to_iforeach
(foldleft: ('xs, 'x0, int) foldleft): 
('xs, 'x0) iforeach =
  fun (xs: 'xs) (work: int -> 'x0 -> unit): unit -> 
    let _ = 
      foldleft(xs)(0)(fun i xi -> (work(i)(xi); i+1))
    in ()
  ;;
