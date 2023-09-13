#use "./../assign0.ml";;

let rec loop(cs: string)(i0: int): int =
  if i0 < 1 then ord(string_get(cs, i0))-48
  else (ord(string_get(cs, i0))-48) + 10*loop(cs)(i0-1)
;;

let str2int(cs: string): int =  
  loop(cs)(string_length(cs)-1)
;;
