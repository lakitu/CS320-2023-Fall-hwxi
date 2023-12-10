#use "./../../../classlib/OCaml/MyOCaml.ml";;
#use "./../../interp2/MySolution/interp2.ml";;

(*

Please implement the [compile] function following the
specifications described in CS320_Fall_2023_Project-3.pdf

Notes:
1. You are only allowed to use library functions defined in MyOCaml.ml
   or ones you implement yourself.
2. You may NOT use OCaml standard library functions directly.

*)

(* ------------------------------------------------------------ *)

(* abstract syntax tree of high-level language *)

type uopr =
  | Neg | Not

type bopr =
  | Add | Sub | Mul | Div | Mod
  | And | Or
  | Lt  | Gt  | Lte | Gte | Eq

type expr =
  | Int of int
  | Bool of bool
  | Unit
  | UOpr of uopr * expr
  | BOpr of bopr * expr * expr
  | Var of string
  | Fun of string * string * expr
  | App of expr * expr
  | Let of string * expr * expr
  | Seq of expr * expr
  | Ifte of expr * expr * expr
  | Trace of expr

(* ------------------------------------------------------------ *)

(* combinator for left-associative operators *)

let chain_left (p : 'a parser) (q : ('a -> 'a -> 'a) parser) : 'a parser =
  let* init = p in
  let* fms = many (let* f = q in let* m = p in pure (f, m)) in
  let m = list_foldleft fms init (fun acc (f, m) -> f acc m) in
  pure m

let rec chain_right (p : 'a parser) (q : ('a -> 'a -> 'a) parser) : 'a parser =
  let* m = p in
  (let* f = q in
   let* rest = chain_right p q in
   pure (f m rest)) <|> 
  (pure m)

let opt (p : 'a parser) : 'a option parser =
  (let* x = p in pure (Some x)) <|> pure None

(* basic constants *)

let parse_int : expr parser =
  let* n = natural in
  pure (Int n) << whitespaces

let parse_bool : expr parser =
  (keyword "true" >> pure (Bool true)) <|>
  (keyword "false" >> pure (Bool false))

let parse_unit : expr parser =
  keyword "(" >> keyword ")" >> pure Unit

(* names *)

let isReserved s =
  let reserved = 
    ["let"; "rec"; "in"; "fun"; "if"; "then"; "else"; "trace"; "mod"; "not"] 
  in
  list_exists reserved (fun s0 -> s0 = s)

let parse_name : string parser =
  let lower = satisfy char_islower in
  let upper = satisfy char_isupper in
  let digit = satisfy char_isdigit in
  let quote = char '\'' in
  let wildc = char '_' in
  let* c = lower <|> wildc in
  let* cs = many (lower <|> upper <|> digit <|> wildc <|> quote) in
  let s = string_make_fwork (list_foreach (c :: cs)) in
  if isReserved s then fail
  else pure s << whitespaces

(* unary operators *)

let parse_neg : (expr -> expr) parser =
  keyword "-" >> pure (fun m -> UOpr (Neg, m))

(* binary operators *)

let parse_add : (expr -> expr -> expr) parser =
  keyword "+" >> pure (fun m n -> BOpr (Add, m, n))

let parse_sub : (expr -> expr -> expr) parser =
  keyword "-" >> pure (fun m n -> BOpr (Sub, m, n))

let parse_mul : (expr -> expr -> expr) parser =
  keyword "*" >> pure (fun m n -> BOpr (Mul, m, n))

let parse_div : (expr -> expr -> expr) parser =
  keyword "/" >> pure (fun m n -> BOpr (Div, m, n))

let parse_mod : (expr -> expr -> expr) parser =
  keyword "mod" >> pure (fun m n -> BOpr (Mod, m, n))

let parse_and : (expr -> expr -> expr) parser =
  keyword "&&" >> pure (fun m n -> BOpr (And, m, n))

let parse_or : (expr -> expr -> expr) parser =
  keyword "||" >> pure (fun m n -> BOpr (Or, m, n))

let parse_lt : (expr -> expr -> expr) parser =
  keyword "<" >> pure (fun m n -> BOpr (Lt, m, n))

let parse_gt : (expr -> expr -> expr) parser =
  keyword ">" >> pure (fun m n -> BOpr (Gt, m, n))

let parse_lte : (expr -> expr -> expr) parser =
  keyword "<=" >> pure (fun m n -> BOpr (Lte, m, n))

let parse_gte : (expr -> expr -> expr) parser =
  keyword ">=" >> pure (fun m n -> BOpr (Gte, m, n))

let parse_eq : (expr -> expr -> expr) parser =
  keyword "=" >> pure (fun m n -> BOpr (Eq, m, n))

let parse_neq : (expr -> expr -> expr) parser =
  keyword "<>" >> pure (fun m n -> UOpr (Not, BOpr (Eq, m, n)))

let parse_seq : (expr -> expr -> expr) parser =
  keyword ";" >> pure (fun m n -> Seq (m, n))

(* expression parsing *)

let rec parse_expr () = 
  let* _ = pure () in
  parse_expr9 ()

and parse_expr1 () : expr parser = 
  let* _ = pure () in
  parse_int <|> 
  parse_bool <|> 
  parse_unit <|>
  parse_var () <|>
  parse_fun () <|>
  parse_letrec () <|>
  parse_let () <|>
  parse_ifte () <|>
  parse_trace () <|>
  parse_not () <|>
  (keyword "(" >> parse_expr () << keyword ")")

and parse_expr2 () : expr parser =
  let* m = parse_expr1 () in
  let* ms = many' parse_expr1 in
  let m = list_foldleft ms m (fun acc m -> App (acc, m)) in
  pure m

and parse_expr3 () : expr parser =
  let* f_opt = opt parse_neg in
  let* m = parse_expr2 () in
  match f_opt with
  | Some f -> pure (f m)
  | None -> pure m

and parse_expr4 () : expr parser =
  let opr = parse_mul <|> parse_div <|> parse_mod in
  chain_left (parse_expr3 ()) opr

and parse_expr5 () : expr parser =
  let opr = parse_add <|> parse_sub in
  chain_left (parse_expr4 ()) opr

and parse_expr6 () : expr parser =
  let opr = 
    parse_lte <|> 
    parse_gte <|>
    parse_neq <|>
    parse_lt <|> 
    parse_gt <|>
    parse_eq
  in
  chain_left (parse_expr5 ()) opr

and parse_expr7 () : expr parser =
  chain_left (parse_expr6 ()) parse_and

and parse_expr8 () : expr parser =
  chain_left (parse_expr7 ()) parse_or

and parse_expr9 () : expr parser =
  chain_right (parse_expr8 ()) parse_seq

and parse_var () : expr parser =
  let* x = parse_name in
  pure (Var x)

and parse_fun () : expr parser =
  let* _ = keyword "fun" in
  let* xs = many1 parse_name in 
  let* _ = keyword "->" in
  let* body = parse_expr () in
  let m = list_foldright xs body (fun x acc -> Fun ("", x, acc)) in
  pure m

and parse_let () : expr parser =
  let* _ = keyword "let" in
  let* x = parse_name in
  let* xs = many parse_name in
  let* _ = keyword "=" in
  let* body = parse_expr () in
  let* _ = keyword "in" in
  let* n = parse_expr () in
  let m = list_foldright xs body (fun x acc -> Fun ("", x, acc)) in
  pure (Let (x, m, n))

and parse_letrec () : expr parser =
  let* _ = keyword "let" in
  let* _ = keyword "rec" in
  let* f = parse_name in
  let* x = parse_name in
  let* xs = many parse_name in
  let* _ = keyword "=" in
  let* body = parse_expr () in
  let* _ = keyword "in" in
  let* n = parse_expr () in
  let m = list_foldright xs body (fun x acc -> Fun ("", x, acc)) in
  pure (Let (f, Fun (f, x, m), n))

and parse_ifte () : expr parser =
  let* _ = keyword "if" in
  let* m = parse_expr () in
  let* _ = keyword "then" in
  let* n1 = parse_expr () in
  let* _ = keyword "else" in
  let* n2 = parse_expr () in
  pure (Ifte (m, n1, n2))

and parse_trace () : expr parser =
  let* _ = keyword "trace" in
  let* m = parse_expr1 () in
  pure (Trace m) 

and parse_not () : expr parser =
  let* _ = keyword "not" in
  let* m = parse_expr1 () in
  pure (UOpr (Not, m))

exception SyntaxError
exception UnboundVariable of string

type scope = (string * string) list

let new_var =
  let stamp = ref 0 in
  fun x ->
    incr stamp;
    let xvar = string_filter x (fun c -> c <> '_' && c <> '\'') in
    string_concat_list ["v"; xvar; "i"; string_of_int !stamp]

let find_var scope s =
  let rec loop scope =
    match scope with
    | [] -> None
    | (s0, x) :: scope ->
      if s = s0 then Some x
      else loop scope
  in loop scope

let scope_expr (m : expr) : expr = 
  let rec aux scope m =
    match m with
    | Int i -> Int i
    | Bool b -> Bool b
    | Unit -> Unit
    | UOpr (opr, m) -> UOpr (opr, aux scope m)
    | BOpr (opr, m, n) -> 
      let m = aux scope m in
      let n = aux scope n in
      BOpr (opr, m, n)
    | Var s -> 
      (match find_var scope s with
       | None -> raise (UnboundVariable s)
       | Some x -> Var x)
    | Fun (f, x, m) -> 
      let fvar = new_var f in
      let xvar = new_var x in
      let m = aux ((f, fvar) :: (x, xvar) :: scope) m in
      Fun (fvar, xvar, m)
    | App (m, n) ->
      let m = aux scope m in
      let n = aux scope n in
      App (m, n)
    | Let (x, m, n) ->
      let xvar = new_var x in
      let m = aux scope m in
      let n = aux ((x, xvar) :: scope) n in
      Let (xvar, m, n)
    | Seq (m, n) ->
      let m = aux scope m in
      let n = aux scope n in
      Seq (m, n)
    | Ifte (m, n1, n2) ->
      let m = aux scope m in
      let n1 = aux scope n1 in
      let n2 = aux scope n2 in
      Ifte (m, n1, n2)
    | Trace m -> Trace (aux scope m)
  in
  aux [] m

(* ------------------------------------------------------------ *)

(* parser for the high-level language *)

let parse_prog (s : string) : expr =
  match string_parse (whitespaces >> parse_expr ()) s with
  | Some (m, []) -> scope_expr m
  | _ -> raise SyntaxError

let rec translate_prog (e: expr): prog =
  match e with
  | Int i -> translate_int i
  | Bool b -> translate_bool b
  | Unit -> translate_unit()
  | UOpr(opr, exp) -> translate_uopr opr exp
  | BOpr(opr, exp1, exp2) -> translate_bopr opr exp1 exp2
  | Var str -> translate_var str
  | Fun(name, arg, body) -> translate_fun name arg body
  | App(funName, arg) -> translate_app funName arg
  | Let(name, value, inExpr) -> translate_let name value inExpr
  | Seq(expr1, expr2) -> translate_seq expr1 expr2
  | Ifte(cond, ifExpr, elseExpr) -> translate_ifte cond ifExpr elseExpr
  | Trace(tExpr) -> translate_trace tExpr

and translate_int (i: int): coms =
  [Push(Integer i)] 

and translate_bool (b: bool): coms =
  [Push(Boolean b)]

and translate_unit(): coms =
  [Push(Unit ())]

and translate_uopr(opr: uopr)(expression: expr): prog =
  let transComsList = translate_prog expression in
  match opr with
  | Not -> list_append (transComsList) [Not]
  | Neg -> list_append (transComsList) [Push(Integer(-1)); Mul]

and translate_bopr(opr: bopr)(expr1: expr)(expr2: expr): coms =
  let stackComs1 = translate_prog expr1 in
  let stackComs2 = translate_prog expr2 in
  match opr with
  | Add -> list_concat [stackComs2; stackComs1; [Add]]
  | Sub -> list_concat [stackComs2; stackComs1; [Sub]]
  | Mul -> list_concat [stackComs2; stackComs1; [Mul]]
  | Div -> list_concat [stackComs2; stackComs1; [Div]]
  | Mod -> list_concat [stackComs2; [Push(Sym "e1"); Bind;];
                stackComs1; [Push(Sym "e2"); Bind;
                Push(Sym "e2"); Lookup;
                Push(Sym "e1"); Lookup;
                Div; Push(Sym "e2"); Lookup; Mul;
                Push(Sym "e1"); Lookup; Sub;]]
  | And -> list_concat [stackComs1; stackComs2; [And]]
  | Or  -> list_concat [stackComs1; stackComs2; [Or] ]
  | Lt  -> list_concat [stackComs2; stackComs1; [Lt] ]
  | Gt  -> list_concat [stackComs2; stackComs1; [Gt] ]
  (* TEST EXTENSIVLEY *)
  | Lte -> list_concat 
            [stackComs2; stackComs1; 
            [Push(Integer 1); Swap; Sub; Lt]]
  | Gte -> list_concat 
            [stackComs2; stackComs1;
            [Push(Integer 1); Add; Gt]]
  | Eq  -> list_concat 
            [stackComs1; stackComs2;
            [Push(Sym "e2"); Bind;
            Push(Sym "e1"); Bind;

            Push(Sym "e1"); Lookup;
            Push(Sym "e2"); Lookup; Lt;
            
            Push(Sym "e1"); Lookup;
            Push(Sym "e2"); Lookup; Gt;
            Or; Not;]]

and translate_var (str: string): coms =
  [Push (Sym str); Lookup];

and translate_fun (name: string)(argName: string)(funBody: expr) =
  let funBody = translate_prog funBody
  |> list_append [Push (Sym argName); Bind] in
  [Push (Sym name); Fun funBody]

and translate_app (name: expr)(arg: expr) =
  let transName = translate_prog name in
  let transArg  = translate_prog arg  in
  list_concat [transArg; transName; [Call]]

and translate_let (name: string)(varVal: expr)(inVal: expr) =
  let varComs = translate_prog varVal in
  let inProg  = translate_prog inVal  in
  list_concat [varComs; [Push(Sym name); Bind;]; inProg]

and translate_seq (expr1: expr)(expr2: expr) =
  let coms1 = translate_prog expr1 in
  let coms2 = translate_prog expr2 in
  list_append coms2 coms1

and translate_ifte (ifExpr: expr)(thenExpr: expr)(elseExpr: expr) =
  let ifComs   = translate_prog ifExpr in
  let thenComs = translate_prog thenExpr in
  let elseComs = translate_prog elseExpr in
  list_append ifComs [IfElse(thenComs, elseComs)]

and translate_trace (traceExpr: expr) =
  let traceComs = translate_prog traceExpr in
  list_append traceComs [Trace]
;;

let rec compile_prog(progAst: com list): string =
  match progAst with
  | Push cnst :: rest -> "Push "^(const_to_string cnst)^";"^compile_prog rest
  | Pop :: rest -> "Pop;"^compile_prog rest
  | Swap :: rest -> "Swap;"^compile_prog rest
  | Trace :: rest -> "Trace;"^compile_prog rest
  | Add :: rest -> "Add;"^compile_prog rest
  | Sub :: rest -> "Sub;"^compile_prog rest
  | Mul :: rest -> "Mul;"^compile_prog rest
  | Div :: rest -> "Div;"^compile_prog rest
  | And :: rest -> "And;"^compile_prog rest
  | Or  :: rest -> "Or;" ^compile_prog rest
  | Not :: rest -> "Not;"^compile_prog rest
  | Lt  :: rest -> "Lt;" ^compile_prog rest
  | Gt  :: rest -> "Gt;" ^compile_prog rest
  | IfElse(coms1, coms2) :: rest -> 
    "If "^compile_prog coms1^
    "Else "^compile_prog coms2^"End;"^
    compile_prog rest
  | Bind :: rest -> "Bind;"^compile_prog rest
  | Lookup :: rest -> "Lookup;"^compile_prog rest
  | Fun func :: rest -> 
    "Fun "^compile_prog func^"End;"^compile_prog rest
  | Call :: rest -> "Call;"^compile_prog rest
  | Return :: rest -> "Return;"^compile_prog rest
  | [] -> ""

let compile (s : string) : string =
  parse_prog s
  |> translate_prog 
  |> compile_prog

let compile_and_run (s: string): t_trace option =
  compile s |> interp