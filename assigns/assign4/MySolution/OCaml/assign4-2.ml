#use "../../../../classlib/OCaml/MyOCaml.ml"

let theNatPairs: (int*int) stream =
  fun () -> 
    let rec streamize (i1: int)(j1: int)(sum: int) = fun () ->
      if j1 = -1 then streamize(0)(sum+1)(sum + 1)()
      else StrCons((i1, j1), streamize(i1+1)(j1-1)(sum))
    in
    StrCons((0, 0), streamize(0)(1)(1))
    ;;