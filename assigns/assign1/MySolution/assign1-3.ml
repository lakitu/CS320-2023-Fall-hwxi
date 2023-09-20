#use "../../classlib/OCaml/MyOCaml.ml";;

let char_is_13 (c1: char)(c2: char): bool = ord(c1) < ord(c2);;

let char_is_32 (c1: char)(c2: char): bool = ord(c1) > ord(c2);;


let string_is_132(cs: string): bool =
  char_is_13(string_get_at(cs)(0))(string_get_at(cs)(2)) &&
  char_is_32(string_get_at(cs)(2))(string_get_at(cs)(1))
;;

let rec find_combo (cs: string)(test: char->char->bool)(c1: char)(i: int): int =
  if i > string_length(cs) then -1
  else if test(c1)(string_get_at(cs)(i)) then i
  else find_combo(cs)(test)(c1)(i+1)
;;

let string_avoid_132 (cs: string): bool =
  

