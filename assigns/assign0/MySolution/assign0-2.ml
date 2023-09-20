let rec isPrimeLoop (n0: int) (i: int): bool =
  if i > n0 then true
  else if n0 mod (i - 1) = 0 then false
  else if n0 mod (i - 5) = 0 then false
  else isPrimeLoop(n0) (i + 6)
;;

let isPrime (n0: int): bool =
  if n0 < 2 then false
  else if n0 <= 3 then true
  else if n0 mod 2 = 0 then false
  else if n0 mod 3 = 0 then false
  else isPrimeLoop(n0) (12)
;;

isPrime(-1);;
(* isPrime(2);;
isPrime(5);;
isPrime(6);;
isPrime(91);;
isPrime(89);; *)