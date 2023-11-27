#use "../../../classlib/OCaml/MyOCaml.ml";;

let rec int2str (i0: int): string =
  if i0 < 10 then str(chr(i0+48))
  else string_snoc(int2str(i0/10))(chr((i0 mod 10) + 48))
;;