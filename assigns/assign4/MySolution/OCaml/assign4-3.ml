#use "../../../../classlib/OCaml/MyOCaml.ml"

type 'a gtree =
| GTnil
| GTcons of 'a * ('a gtree list)
;;

(* let remove_overlap(xs: 'a list)(ys: 'a gtree list) =
  list_foldright(xs)([])
  (fun x0 r0 -> 
    if list_exists(ys)(fun yi ->
      match yi with
      | GTnil -> false
      | GTcons(v, n) -> v = x0
    ) then r0
    else x0 :: r0
  )
;; *)

let list_append(xs: 'a list)(ys: 'a list): 'a list =
  list_foldright(xs)(ys)(fun x0 r0 -> x0 :: r0)
;;

let rec gtree_streamize_dfs(xs: 'a gtree): 'a stream =
  fun () -> 
    let rec streamize(stack: 'a gtree list) = fun () ->
      match stack with
      | [] -> StrNil
      | xh :: xt -> (
        match xh with
        | GTnil -> streamize(xt)()
        | GTcons(v, neighbors) -> (
          let newStack = list_append(neighbors)(xt) in
          StrCons(v, streamize(newStack))
        )
      )
    in streamize([xs])()
  ;;

let rec gtree_streamize_bfs(xs: 'a gtree): 'a stream = 
  fun () -> 
    let rec streamize(queue: 'a gtree list) = fun () -> 
      match queue with
      | [] -> StrNil
      | xh :: xt -> 
        match xh with
        | GTnil -> streamize(xt)()
        | GTcons(v, neighbors) -> 
          let newQueue = list_append(xt)(neighbors) in
          StrCons(v, streamize(newQueue))
    in streamize([xs])()
  ;; 

