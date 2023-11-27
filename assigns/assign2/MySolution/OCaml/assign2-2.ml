#use "../../assign2.ml";;

let rec mylist_length(xs: 'a mylist): int =
  match xs with
  | MyNil -> 0
  | MyCons(xi, xs) -> 1 + mylist_length(xs)
  | MySnoc(xs, xi) -> 1 + mylist_length(xs)
  | MyAppend2(xs1, xs2) -> mylist_length(xs1) + mylist_length(xs2)
  | MyReverse(xs) -> mylist_length(xs)
;;


let rec mylist_get_at(xs: 'a mylist)(i0: int): 'a = 
  if i0 < 0 then mylist_subscript_exn() else
  match xs with 
  | MyNil -> mylist_subscript_exn()
  | MyCons(xi, xs) -> 
      if i0 = 0 then xi
      else mylist_get_at(xs)(i0-1)
  | MySnoc(xs, xi) -> 
      if i0 = 0 then xi
      else mylist_get_at(xs)(i0-1)
  | MyAppend2(xs1, xs2) -> 
      let list1_len = mylist_length(xs1) in
      if list1_len > i0 then
        mylist_get_at(xs1)(i0)
      else
        mylist_get_at(xs2)(i0-list1_len)
  | MyReverse(xs) -> mylist_get_at(xs)(mylist_length(xs)-i0-1)
;;