#use "../../../classlib/OCaml/MyOCaml.ml";;

let last_char (ds: string): char =
  if string_length(ds) = 0 then '0'
  else string_get_at(ds)(string_length(ds)-1)
;;

let all_but_tail (ds: string): string = 
  if string_length(all_but_tail) = 0 then ""
  else string_init(string_length(ds)-1)(fun i -> string_get_at(ds)(i))
;;
  

let rec intrep_add_helper(ds1: string)(ds2: string)(carry: int)(acc: string): string =
  if ds1 = "" && ds2 = "" then (
    if carry = 1 then 
      string_cons('1')(acc)
    else acc
  )
  else (
    let last_digit_1 = ds1 |> last_char |> digit_of_char in
    let last_digit_2 = ds2 |> last_char |> digit_of_char in
    let sum = last_digit_1 + last_digit_2 + carry in
    intrep_add_helper(all_but_tail(ds1))(all_but_tail(ds2))
    (if sum >= 10 then 1 else 0)
    (string_cons(char_of_digit(sum mod 10))(acc))
  )
;;

#trace intrep_add_helper;;

let intrep_add(ds1: string)(ds2: string): string = 
  intrep_add_helper(ds1)(ds2)(0)("")
;;