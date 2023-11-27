#use "../../../classlib/OCaml/MyOCaml.ml";;

let get_last_char(str: string): char = 
  if str = "" then chr(0)
  else string_get_at(str)(string_length(str)-1)
;;

let str_of_char c = string_cons(c)("");;

let string_longest_ascend(xs: string): string =
  let rec longest_ascend_helper(i: int)(streak: string)(longest: string): string =
    if i >= string_length(xs) then (
      let _ = print_endline(longest) in
      let _ = print_endline(streak) in
      if string_length(streak) >= string_length(longest)
        then streak 
        else longest
    )
    else if i = 0 then (
      let str_of_head = string_cons(string_head(xs))("") in
      longest_ascend_helper(i+1)(str_of_head)(str_of_head)
    )
    else (
      let c = string_get_at(xs)(i) in
      if get_last_char(streak) <= c then (
        let f = longest_ascend_helper(i+1)(string_snoc(streak)(c)) in
        if get_last_char(longest) <= c then
          f(string_snoc(longest)(c))
        else f(longest)
      )
      else (
        if string_length(longest) <= string_length(streak) then
          longest_ascend_helper(i+1)(str_of_char(c))(streak)
        else if get_last_char(longest) <= c then
          longest_ascend_helper(i+1)(str_of_char(c))(string_snoc(longest)(c))
        else 
          longest_ascend_helper(i+1)(str_of_char(c))(longest)
      )
    )
  in
  longest_ascend_helper(0)("")("")
;;