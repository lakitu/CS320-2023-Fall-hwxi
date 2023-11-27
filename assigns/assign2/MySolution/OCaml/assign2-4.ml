#use "../../assign2.ml";;
#use "./../../MySolution/OCaml/assign2-2.ml";;
#use "../../../../classlib/OCaml/MyOCaml.ml";;

(*
Please given a combinator-based implementation of
string_sepjoin_list:
let
string_sepjoin_list
(sep: string)(xs: string list): string = ...

For instance,
string_sepjoin_list("")(["1";"22";"333"]) = "122333"
For instance,
string_sepjoin_list(",")(["1";"22";"333"]) = "1,22,333"
For instance,
string_sepjoin_list(";;")(["11";"22";"33"]) = "11;;22;;33"
*)

let rec string_head_removal_service (str)(n) = 
  if n = 0 then str
  else string_head_removal_service(string_tail(str))(n-1)
;;

let string_sepjoin_list (sep: string)(xs: string list): string =
  let append = fun x0 r0 ->
    let with_sep = string_append(sep)(r0) in 
    string_append(x0)(with_sep)
  in
  string_head_removal_service(list_foldleft(xs)("")(append))(string_length(sep))
;;
