#use "../../../../classlib/OCaml/MyOCaml.ml";;

exception IndexOutOfBounds;;

let rec list_get_at(xs:'a list)(i:int): 'a =
  match xs with
  | [] -> raise IndexOutOfBounds
  | x1 :: xs -> 
    if i = 0 then x1 
    else list_get_at(xs)(i-1)
  ;;

let rec list_length(list:'a list)(sum:int):int =
  match list with
  | [] -> sum
  | head :: tail -> list_length(tail)(sum+1)
;;

let rec
matrix_transpose(xss: 'a list list): 'a list list =
  let outer_list_len:int = list_length(xss)(0) in
  let inner_list_len:int = list_length(list_get_at(xss)(0))(0) in

  list_make_fwork(fun outerWork -> 
    let rec outerLoop i =
      if i >= inner_list_len then ()
      else (outerWork(
        list_make_fwork(fun innerWork -> 
          let rec innerLoop j =
            if j >= outer_list_len then ()
            else (
              innerWork(list_get_at(list_get_at(xss)(j))(i));
              innerLoop(j+1)
            ) in
          innerLoop(0)
        )); outerLoop(i+1))
      in outerLoop(0)
  )