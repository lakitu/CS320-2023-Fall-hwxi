#use "../../../../classlib/OCaml/MyOCaml.ml";;

let allLetters = 
  list_make_fwork(fun work -> 
    int1_foreach(26)(fun i -> 
      work(chr(97+i))
    )    
  )
;;

let replace(word:string)(i:int)(c:char):string =
  string_make_fwork(fun work -> 
    let list = string_rlistize(word) in
    let _ = 
    list_foldright(list)(0)(fun xi r0 -> (
      if (i = r0) then work(c)
      else work(xi)
    ); r0+1) in ()
  )
;;

let list_of_buddies(word: string): string list =
  list_make_fwork(fun work -> 
    let string_list = string_listize(word) in
    let _ = list_foldright(string_list)(0)(
      fun letter i -> (
        list_foreach(allLetters)(fun c -> 
          if letter != c then work(replace(word)(i)(c))
          else ()
        ); i+1)
    )
    in ()
  )
;;