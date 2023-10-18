#use "../../../../classlib/OCaml/MyOCaml.ml"

let list_len (xs: 'a list): int =
  list_foldleft(xs)(0)(fun r0 xi -> r0 + 1)
;;

let fac (b: int): int =
  list_foldleft(int1_listize(b))(1)(fun r0 xi -> r0 * (xi + 1))
;;

let list_iforeach(arr: 'a list)(iwork: int -> 'a -> unit): unit = 
  let _ = list_foldleft(arr)(0)(fun i x -> (iwork(i)(x); i+1)) in ()
;;

let list_riforeach(arr: 'a list)(iwork: int->'a->unit): unit =
  let _ = list_foldright(arr)(0)(fun x i -> iwork(i)(x); i+1) 
  in ()
;;

exception IndexOutOfBounds;;
(* I couldn't figure out a way to make the exception Found raise with the generic type 'a *)
let list_get_at(arr: int list)(i: int): int =
  let exception Found of int in
  try (
    list_iforeach(arr)(fun j x -> if i = j then raise(Found x)); 
    raise IndexOutOfBounds
  )
  with Found xi -> xi
;;

let list_remove_at(arr: 'a list)(i: int): 'a list =
  list_make_fwork(fun work ->
    let _ = list_foldleft(arr)(0)(fun r0 x0 -> (if r0 != i then work(x0)); r0 + 1)
    in ()
  )
;;

let list_permute(xs: 'a list): 'a list stream = fun () ->
  let len = list_len(xs) in
  let facLen = fac(len) in
  let divisions = 
    list_remove_at(
      list_make_fwork(fun work -> 
        int1_rforeach(len+1)(fun xi -> xi|>fac|>work)
      )
    )(len) in
  (* let _ = print_list(divisions) in *)
  let rec streamize(count: int) = fun () -> 
    let permutation = list_make_fwork(fun work ->
      let rec makeList(xs: 'a list)(soFar: int): unit =
        if soFar = len then ()
        else (
          let fi = list_get_at(divisions)(soFar) in
          let fp = list_get_at(divisions)(soFar-1) in
          if fi = 1 then
            if count mod 2 = 0 then 
              (work(list_get_at(xs)(0)); work(list_get_at(xs)(1)))
            else 
              (work(list_get_at(xs)(1)); work(list_get_at(xs)(0)))
          else
            let i = (count mod fp) / fi in
            let _ = work(list_get_at(xs)(i)) in
            let newxs = list_remove_at(xs)(i) in
            makeList(newxs)(soFar+1)
        )
      in makeList(xs)(1)
    ) in
    if count = facLen then StrNil
    else StrCons(permutation, streamize(count+1))
  in streamize(0)()
;;