(* requires: n >= 0 *)
let rec fac n =
  if n <= 0 then 1
  else n * fac (n-1)
;;

let rec testfac (n) = 
  if fac(n) = 0 then n
  else testfac (n+1)
;;

(* let myans = testfac(1) *)

let myans = 64;