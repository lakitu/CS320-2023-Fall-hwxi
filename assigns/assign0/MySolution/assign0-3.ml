(* ****** ****** *)
let chr = Char.chr
let ord = Char.code
let str(c0) = String.make 1 c0
;;
(* ****** ****** *)
let string_init = String.init
let string_length = String.length
let string_get(cs, i0) = String.get cs i0
;;
(* ****** ****** *)
let rec int2str (i0: int): string =
  if i0 < 10 then str(chr(i0+48))
  else int2str(i0/10) ^ str(chr((i0 mod 10) + 48))
;;
