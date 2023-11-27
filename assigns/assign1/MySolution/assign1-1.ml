let rec intrev10Helper(n: int)(newVal: int) =
  if n < 10 then newVal * 10 + n
  else intrev10Helper(n / 10)((newVal * 10) + (n mod 10))
;;

let rec intrev10 (n: int) =
intrev10Helper(n)(0)
;;