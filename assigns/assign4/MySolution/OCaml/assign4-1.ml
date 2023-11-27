#use "../../../../classlib/OCaml/MyOCaml.ml"

let sign (e:int): float =
  if (e mod 2) = 0 then -1.
  else 1.;;
;;

let the_ln2_stream: float stream =
  fun () -> 
    let rec streamize (currNum: int)(sum: float) = fun () ->
      let next = sign(currNum) /. float_of_int(currNum) in
      let currSum = sum +. next in
      StrCons(currSum, streamize(currNum+1)(currSum))
    in
    StrCons(1., streamize(2)(1.))
    ;;