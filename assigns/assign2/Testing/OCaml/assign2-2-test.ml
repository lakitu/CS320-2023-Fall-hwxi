(* ****** ****** *)
#use "./../../MySolution/OCaml/assign2-2.ml";;
<<<<<<< HEAD
(* #use "../../assign2-ml";; *)
(* ****** ****** *)
let xs0 = MyNil
let xs1 = MyCons(1, xs0)
let xs2 = MySnoc(xs0, 1)
=======
(* ****** ****** *)
let xs0 = MyNil
let xs1 = MyCons(10, xs0)
let xs2 = MySnoc(xs0, -10)
>>>>>>> upstream/main
let xs3 = MyAppend2(xs1, xs2)
let xs4 = MyReverse(xs3)
let xs5 = MyAppend2(xs4, xs4)
let xs6 = MyAppend2(xs5, xs5)
let xs7 = MyAppend2(xs6, xs6)
;;
<<<<<<< HEAD

let l0 = MyNil
let l1 = MyCons(1, l0)
let l2 = MySnoc(l0, 2)
let l21 = MyAppend2(l2, l1)
let l12 = MyReverse(l21)
let l3 = MyCons(3, l0)
let l4 = MySnoc(l0, 4)
let l5 = MySnoc(l0, 5)
let l6 = MyCons(6, l0)
let l7 = MyCons(7, l0)
let l8 = MyCons(8, l0)
let l43 = MyAppend2(l4, l3)
let l65 = MyAppend2(l6, l5)
let l6543 = MyAppend2(l65, l43)
let l3456 = MyReverse(l6543)
let l78 = MyReverse(MyReverse(MyAppend2(l7, l8)))
let l3t8 = MyAppend2(l3456, l78)
let l1t8 = MyAppend2(l12, l3t8)
;;
(* ****** ****** *)
let () =
assert(mylist_get_at(l1t8)(1) = 2)
let () = 
assert(mylist_get_at(l1t8)(5) = 6)
let () = match mylist_get_at(MyNil)(0) with 
| mylist_subscript_exn() -> true | _ -> assert(false)
let () = match mylist_get_at(l1t8)(-1) with
| mylist_subscript_exn() -> true | _ -> assert(false)
(* ****** ****** *)
let () =
print_string("Assign2-1-test passed!\n")
(* ****** ****** *)
;;(* end-of-let *)
(* ****** ****** *)

(* end of [CS320-2023-Fall-assign2-1-test.sml] *)
=======
(* ****** ****** *)
let ( ) = assert(10 = mylist_get_at(xs7)(1))
let ( ) = assert(10 = mylist_get_at(xs7)(3))
let ( ) = assert(10 = mylist_get_at(xs7)(5))
let ( ) = assert(10 = mylist_get_at(xs7)(7))
;;
(* ****** ****** *)
let ( ) = assert(10 = -mylist_get_at(xs7)(0))
let ( ) = assert(10 = -mylist_get_at(xs7)(2))
let ( ) = assert(10 = -mylist_get_at(xs7)(4))
let ( ) = assert(10 = -mylist_get_at(xs7)(6))
;;
(* ****** ****** *)
let () = print_string("Assign2-2-test passed!\n");;
(* ****** ****** *)

(* end of [CS320-2023-Fall-assign2-2-test.sml] *)
>>>>>>> upstream/main
