#use "./../../classlib/OCaml/MyOCaml.ml";;
(*

Grammar (<expr> is the start symbol)

<digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
<num>   ::= <digit> | <digit><num>
<expr>  ::= <num> 
          | (add <exprs> )
          | (mul <exprs> )
<exprs> ::= <expr> | <expr><exprs>
*)

type expr =
  | Int of int       (* 1, 2, 3, 4 ...  *)
  | Add of expr list (* (add e1 e2 ...) *)
  | Mul of expr list (* (mul e1 e2 ...) *)

(* turn a string into a list of chars *)
let string_listize (s : string) : char list =
  list_make_fwork(fun work -> string_foreach s work)

(* remove blank chars at the front of a list *)
let rec trim cs =
  match cs with
  | [] -> cs
  | '\n' :: cs -> trim cs
  | '\t' :: cs -> trim cs
  | '\r' :: cs -> trim cs
  | ' ' :: cs -> trim cs
  | _ -> cs

let rec trim_list cs: char list =
  match cs with
  | ' ' :: xs -> trim_list xs
  | _ -> cs
;;

(* Please implement a parse function. When given a valid string according
   to the grammar, your parse function returns an expr value encoding the
   expression.

   Example (Accepted Strings):
   parse "(add 1 2 3)" = Some (Add [Int 1; Int 2; Int 3])
   parse "(mul (add 1 2) 3 (mul 1))" = Some (Mul [Add [Int 1; Int 2]; Int 3; Mul [Int 1]])

   Example (Rejected Strings):
   parse "()" = None
   parse "(add)" = None
   parse "(add 1 2))" = None
   parse "((mul 1 2)" = None

*)
let parse_digit(c: char): int option =
  let num = ord(c) - ord('0') in
  if num < 0 || num > 9 then None
  else Some(num)
;;

let parse_num(s: char list): (int * char list) option =
  (* let _ = print_endline("num: " ^ string_constructor s) in *)
  let rec helper(s: char list)(acc: int): (int * char list) option =
    match s with
    | ')' :: xs -> Some(acc, s)
    | ' ' :: xs -> Some(acc, s)
    | d :: xs -> (
      match parse_digit(d) with
      | Some(n) -> helper(xs)(acc*10+n)
      | None -> None  )
    | _ -> None
  in helper(s)(0)
;;

let rec parse_expr(s: char list): (expr * char list) option =
  let parse_exprs(s: char list): (expr list * char list) option =
    let rec helper(acc: (expr list * char list)): (expr list * char list) option =
      (* let _ = print_endline("helper: " ^ string_constructor s) in *)
      let acc, s = acc in
      match s with
      | ')' :: xs -> Some(acc, trim_list xs)
      | '(' :: xs -> (
        match parse_expr(s) with
        | Some(e, xss) -> helper(e :: acc, xss)
        | None -> None  )
      | _ -> (
        match parse_num s with
        | Some(num, xs) ->
            helper(Int(num) :: acc, trim_list xs)
        | None -> None  )
    in match helper([], s) with
    | Some(exprs, xs) -> Some(reverse_list exprs, xs)
    | None -> None
  in
  match s with
  | '(' :: 'a' :: 'd' :: 'd' :: ' ' :: xs -> (
    match parse_exprs(xs) with
    | None -> None
    | Some(es, xss) -> Some(Add(es), xss) )
  | '(' :: 'm' :: 'u' :: 'l' :: ' ' :: xs -> (
    match parse_exprs(xs) with
    | None -> None
    | Some(es, xss) -> Some(Mul(es), xss) )
  | _ -> None
;;

let parse (s : string) : expr option = 
  let cs = s |> string_listize |> trim in
  match parse_expr cs with
  | Some(e, xs) -> (
    match xs with
    | [] -> Some e
    | _ -> None  )
  | None -> None
;;

print_endline("Hello");;