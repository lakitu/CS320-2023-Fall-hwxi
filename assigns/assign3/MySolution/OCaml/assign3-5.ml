#use "../../../../classlib/OCaml/MyOCaml.ml";;
#use "./../../MySolution/OCaml/assign3-3.ml";;

type board_t =
int * int * int * int * int * int * int * int
;;

let collision (q1: int)(q2:int): bool =
  let x1 = q1 mod 8 in
  let y1 = q1 / 8 in
  let x2 = q2 mod 8 in
  let y2 = q2 / 8 in
  let same_file = x1 = x2 in
  let same_rank = y1 = y2 in
  if same_file || same_rank then true
  else
  let num = abs (y2-y1) in
  let den = abs (x2-x1) in
  num = den
;;

let list_collision (xs: int list): bool =
  match xs with
  | [] -> false
  | x1 :: x2s -> 
    match x2s with 
    | [] -> false
    | x2 :: tail -> collision(x1)(x2)
  ;;

let works (allPos: int list): bool =
  let allCombos = list_nchoose(allPos)(2) in
  list_forall(allCombos)
  (fun xs -> not (list_collision(xs)))
;;

let print_arr (xs: int list) =
  let () = list_foreach(xs)
    (fun xi -> (
      print_string(string_of_int(xi)); 
      print_string(", ")
    )) in
  let () = print_endline("") in
  ()
;;

let list_remove (xs: int list)(r: int) = 
  list_foldright(xs)([])(fun xi r0 -> if xi != r then xi :: r0 else r0)
;;

let queen8_puzzle_solve(): board_t list =
  list_make_fwork(fun outerWork -> 
      let allPos = [] in
      int1_foreach(8)(fun x0 -> 
        let t0 = x0 + 0 in let allPos = t0 :: allPos in
      int1_foreach(8)(fun x1 -> 
        let t1 = x1 + 8 in let allPos = t1 :: allPos in
        if (works(allPos)) then
      int1_foreach(8)(fun x2 -> 
        let t2 = x2 + 16 in let allPos = t2 :: allPos in
        if (works(allPos)) then
      int1_foreach(8)(fun x3 -> 
        let t3 = x3 + 24 in let allPos = t3 :: allPos in
        if (works(allPos)) then 
      int1_foreach(8)(fun x4 -> 
        let t4 = x4 + 32 in let allPos = t4 :: allPos in
        if (works(allPos)) then
      int1_foreach(8)(fun x5 -> 
        let t5 = x5 + 40 in let allPos = t5 :: allPos in
        if (works(allPos)) then
      int1_foreach(8)(fun x6 -> 
        let t6 = x6 + 48 in let allPos = t6 :: allPos in
        if (works(allPos)) then
      int1_foreach(8)(fun x7 -> 
        let t7 = x7 + 56 in let allPos = t7 :: allPos in
        if works(allPos) then (
          outerWork((t0,t1,t2,t3,t4,t5,t6,t7));
          print_arr(allPos)
        )
       else (())
      ))))))))
  )
;;

queen8_puzzle_solve();;