#use "./../assign0.ml";;

let rec stringrev(cs: string): string =
  string_init(string_length(cs))
  (fun i -> string_get(cs, string_length(cs)-1-i))
;;

(* 
   let str = "abcdefg";;
   stringrev(str) -> "gfedcab"

  
*)