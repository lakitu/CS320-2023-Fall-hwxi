#use "../../../../classlib/OCaml/MyOCaml.ml";;
#use "../../../assign2/assign2.ml";;                                                             

let in_list (xs: 'a list)(x: 'a): bool =
  not (list_forall(xs)(fun xi -> xi != x))
;;

let list_len (xs: 'a list) =
  list_foldleft(xs)(0)(fun r0 xi -> r0 + 1)
;;

let pow b e =
  list_foldleft(int1_listize(e))(1)(fun r0 xi -> r0 * b)
;;

let
foldleft_to_iforeach
( foldleft
: ('xs,'x0,int) foldleft): ('xs,'x0) iforeach =
fun xs iwork ->
let _ = foldleft(xs)(0)(fun i x -> (iwork(i)(x); i+1)) in ()                           
(* ****** ****** *)
let
list_iforeach =
fun xs -> foldleft_to_iforeach(list_foldleft)(xs)

let rec list_subsets (xs: 'a list): 'a list list =
  list_make_fwork(fun outerWork -> 
    list_foreach
      (int1_listize(pow(2)(list_len(xs))))
      (fun i -> outerWork(
      list_make_fwork(fun innerWork -> 
        list_iforeach(xs)(fun j xj ->
          if i land pow(2)(j) > 0 then
          innerWork(xj)
          else ()
        )
      )
    ))
  )
;;