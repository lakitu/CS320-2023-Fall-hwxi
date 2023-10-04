#use "../../../../classlib/OCaml/MyOCaml.ml";;

let in_list (xs: 'a list)(x: 'a): bool =
  not (list_forall(xs)(fun xi -> xi != x))
;;

let list_len (xs: 'a list) =
  list_foldleft(xs)(0)(fun r0 xi -> r0 + 1)
;;

let pow b e =
  list_foldleft(int1_listize(e))(1)(fun r0 xi -> r0 * b)
;;

let rec list_subsets (xs: 'a list): 'a list list =
  list_make_fwork(fun outerWork -> 
    list_foreach(xs)(fun xi -> outerWork(
      list_make_fwork(fun innerWork -> 
        for i0 = 1 to pow(2)(list_len(xs)-1) do 
          innerWork(xi)
        done
      )
    ))
  )
;;