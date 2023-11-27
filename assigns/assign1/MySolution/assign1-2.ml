#use "../../../classlib/OCaml/MyOCaml.ml";;

(* let string_merge (cs1: string)(cs2: string) : string =  
  let cs1_head: char = cs1 |> string_head
  let cs2_head: char = cs2 |> string_head
  let cs1_tail: string = cs1 |> string_tail in
  let cs2_tail: string = cs2 |> string_tail in
  string_make_fwork(
    fun work -> 
      if cs1 = "" then cs2 
      else if cs2 = "" then cs1 
      else if ord(cs1_head) < ord(cs2_head) then work(cs1_head); string_merge(cs1_tail)(cs2) 
      else if ord(cs2_head) < ord(cs1_head) then cs2_head; string_merge(cs1)(cs2_tail) 
      else work(cs1_head); work(cs2_head); string_merge(cs1_tail)(cs2_tail)
  )
;; *)

let string_merge (cs1: string)(cs2: string) : string =
  string_make_fwork(
    (fun work -> 
      let rec string_merge_recursive (i: int)(j: int) =
        if i >= string_length(cs1) && j >= string_length(cs2) then ()
        else if i >= string_length(cs1) then (
          work(string_get_at(cs2)(j));
          string_merge_recursive(i)(j+1)
        )
        else if j >= string_length(cs2) then (
          work(string_get_at(cs1)(i));
          string_merge_recursive(i+1)(j)
        )
        else (
          let c1 = string_get_at(cs1)(i) in
          let c2 = string_get_at(cs2)(j) in
          if c1 <= c2 then (work(c1); string_merge_recursive(i+1)(j))
          else (work(c2); string_merge_recursive(i)(j+1))
        )
      in 
      string_merge_recursive(0)(0)
    )
  )
;;
