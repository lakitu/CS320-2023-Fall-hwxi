#use "../../../../classlib/OCaml/MyOCaml.ml";;

(* let sexpr_to_string (e : sexpr)  : string       = ... *)
(* let sexpr_parse     (s : string) : sexpr option = ... *)

(*

Grammar (<expr> is the start symbol)

<digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
<num>   ::= <digit> | <digit><num>
<exprs> ::= <expr> | <expr> <exprs>
<expr>  ::= <num>
          | (add <exprs> )
          | (mul <exprs> )
*)

type sexpr =
  | SInt of int        (* 1, 2, 3, 4 ...  *)
  | SAdd of sexpr list (* (add e1 e2 ...) *)
  | SMul of sexpr list (* (mul e1 e2 ...) *)
;;
(* ****** ****** *)

let rec sexpr_parser() =
  (
    let* _ = whitespaces in
    let* _ = keyword "(add" in
    let* terms = many(sexpr_parser()) in
    let* _ = char ')' in
    let* _ = whitespaces in
    pure(SAdd(terms))
  )
  <|>
  (
    let* _ = whitespaces in
    let* _ = keyword "(mul" in
    let* terms = many(sexpr_parser()) in
    let* _ = char ')' in
    let* _ = whitespaces in
    pure(SMul(terms))
  )
  <|>
  (
    let* _ = whitespaces in
    let* x = natural in
    let* _ = whitespaces in
    pure(SInt x)
  )
  <|>
  fail
;;

let sexpr_parse s = 
  string_parse (sexpr_parser()) s
;;

let rec sexpr_to_string (e: sexpr) : string =
  match e with
  | SAdd selist -> (
    let strlist = list_foldleft(selist)("")
    (fun r0 x0 -> 
        r0 ^ (sexpr_to_string x0) ^ " "
    ) in
    "(add " ^ strlist ^ ")"
  )
  | SMul selist -> (
    let strlist = list_foldleft(selist)("")
    (fun r0 x0 -> 
        r0 ^ (sexpr_to_string x0) ^ " "
    ) in
    "(mul " ^ strlist ^ ")"
  )
  | SInt num -> int_to_string(num)
    ;;

(* end of [CS320-2023-Fall-assigns-assign6.ml] *)
