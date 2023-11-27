#use "./../../../classlib/OCaml/MyOCaml.ml";;
(* No other library imports are allowed. *)

(* ************************************************ *)

(* Question 7: 10 points

   Given the following snippet, implement the test
   function so that isPrime returns true for prime
   number inputs and false otherwise. *)

(* 
let chr = Char.chr;;
let ord = Char.code;;

let char_of_digit (d0: int): char =
  let () = assert(d0 >= 0) in
    let () = assert(d0 <= 9) in 
      chr(ord('0') + d0)
;;
let digit_to_str i = i |> char_of_digit |> String.make(1);;
 *)

let isPrime(n) =
  let test(i:int): bool = 
    if i = 0 || i = 1 then true
    else if n mod i = 0 then false
    else true
  in
  if n < 2 then false else int1_forall(n)(test)
;;

(* ************************************************ *)
