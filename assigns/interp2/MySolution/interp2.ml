#use "./../../../classlib/OCaml/MyOCaml.ml";;

(*

Please implement the interp function following the
specifications described in CS320_Fall_2023_Project-2.pdf

Notes:
1. You are only allowed to use library functions defined in MyOCaml.ml
   or ones you implement yourself.
2. You may NOT use OCaml standard library functions directly.

*)

type const = Integer of int
            | Boolean of bool
            | Unit of unit
            | Sym of string
and com = Push of const | Pop | Swap
         | Trace | Add | Sub | Mul | Div
         | And | Or | Not | Lt | Gt
         | IfElse of coms * coms
         | Bind | Lookup | Fun of coms
         | Call | Return
and coms = Coms of com list
and prog = coms
;;

let integer_parser(): const parser =
   (  let* _ = char '-' in 
      let* x = natural in
      pure(Integer(-x)) ) <|>
   (  let* x = natural in
      pure(Integer x)   )

let boolean_parser(): const parser =
   (let* _ = keyword "True" in pure(Boolean true)) <|>
   (let* _ = keyword "False" in pure(Boolean false))

let unit_parser(): const parser =
   let* _ = keyword "Unit" in pure(Unit ())

let isCharOrDigit c =
   (c >= 'a' && c <= 'z') ||
   (c >= '0' && c <= '9')
let sym_parser(): const parser = 
   fun clist -> 
      match many1 (satisfy isCharOrDigit) clist with
      | Some(symParts, rest) -> 
         Some(Sym (string_constructor symParts), rest)
      | _ -> None

let const_parser(): const parser =
   let* _ = whitespaces in
   integer_parser() <|>
   boolean_parser() <|>
   unit_parser() <|>
   sym_parser() <|>
   fail

let rec com_parser(): com parser =
   let* _ = whitespaces in
   (  let* _ = keyword "Push" in
      let* v = const_parser() in
      let* _ = char ';' in
      pure(Push v)  ) <|>
   (let* _ = keyword "Pop;" in pure(Pop)) <|>
   (let* _ = keyword "Swap;" in pure(Swap)) <|>
   (let* _ = keyword "Trace;" in pure(Trace)) <|>
   (let* _ = keyword "Add;" in pure(Add)) <|>
   (let* _ = keyword "Sub;" in pure(Sub)) <|>
   (let* _ = keyword "Mul;" in pure(Mul)) <|>
   (let* _ = keyword "Div;" in pure(Div)) <|>
   (let* _ = keyword "And;" in pure(And)) <|>
   (let* _ = keyword "Or;" in pure(Or)) <|>
   (let* _ = keyword "Not;" in pure(Not)) <|>
   (let* _ = keyword "Lt;" in pure(Lt)) <|>
   (let* _ = keyword "Gt;" in pure(Gt)) <|>
   (  let* _ = keyword "If" in
      let* coms1 = many (com_parser()) in let* _ = whitespaces in
      let* _ = keyword "Else" in
      let* coms2 = many (com_parser()) in let* _ = whitespaces in
      let* _ = keyword "End;" in 
      pure(IfElse(Coms coms1, Coms coms2)) ) <|>
   (let* _ = keyword "Bind;" in pure(Bind)) <|>
   (let* _ = keyword "Lookup;" in pure(Lookup)) <|>
   (  let* _ = keyword "Fun" in
      let* func = many (com_parser()) in 
      let* _ = whitespaces in let* _ = keyword "End;" in
      pure(Fun (Coms func)) ) <|>
   (let* _ = keyword "Call;" in pure(Call)) <|>
   (let* _ = keyword "Return;" in pure(Return)) <|>
   fail

let prog_parser()  =
   many (com_parser())
;;

type t_stack = value list
and t_trace = string list
and t_vars  = (string * value) list
and value = 
| Const of const 
| Closure of closure
and closure = string * t_vars * coms
type com_var_interpreter = t_stack * t_trace * t_vars

let interpret_push(cnst: const)(stack: t_stack)(trace: t_trace)(vars: t_vars): com_var_interpreter =
   (Const cnst :: stack, trace, vars)

let interpret_pop(stack: t_stack)(trace: t_trace)(vars: t_vars): com_var_interpreter =
   match stack with
   | v :: stack -> (stack, trace, vars)
   | [] -> (stack, "Panic" :: trace, vars)

let interpret_swap(stack: t_stack)(trace: t_trace)(vars: t_vars): com_var_interpreter =
   match stack with
   | c1 :: c2 :: stack -> (c2 :: c1 :: stack, trace, vars)
   | _ -> (stack, "Panic" :: trace, vars)

let const_to_string(cnst: const) =
   match cnst with
   | Integer i -> int_to_string i
   | Boolean b ->  boolean_to_string b
   | Unit(()) -> "Unit"
   | Sym str -> str
let interpret_trace(stack: t_stack)(trace: t_trace)(vars: t_vars): com_var_interpreter =
   match stack with
   | Const cnst :: stack -> 
      (Const(Unit ()) :: stack, const_to_string(cnst) :: trace, vars)
   | _ -> ([], "Panic" :: trace, vars)

let interpret_arithmetic(f: int -> int -> int)(stack: t_stack)(trace: t_trace)(vars: t_vars): com_var_interpreter =
   match stack with
   | Const(Integer i1) :: Const(Integer i2) :: stack -> 
      (Const(Integer(f i1 i2)) :: stack, trace, vars)
   | _ -> ([], "Panic" :: trace, vars)

let interpret_add = interpret_arithmetic ( + )
let interpret_sub = interpret_arithmetic ( - )
let interpret_mul = interpret_arithmetic ( * )
let interpret_div(stack: t_stack)(trace: t_trace)(vars: t_vars) = 
   try interpret_arithmetic ( / )(stack)(trace)(vars) with 
   | Division_by_zero -> ([], "Panic" :: trace, vars)

let interpret_bool_op(f: bool -> bool -> bool)
   (stack: t_stack)(trace: t_trace)(vars: t_vars): com_var_interpreter =
   match stack with
   | Const(Boolean a) :: Const(Boolean b) :: stack ->
      (Const(Boolean(f a b)) :: stack, trace, vars)
   | _ -> ([], "Panic" :: trace, vars)

let interpret_and = interpret_bool_op ( && )
let interpret_or = interpret_bool_op ( || )

let interpret_not(stack: t_stack)(trace: t_trace)(vars: t_vars): com_var_interpreter =
   match stack with
   | Const(Boolean a) :: stack -> 
      (Const(Boolean(not a)) :: stack, trace, vars)
   | _ -> ([], "Panic" :: trace, vars)

let interpret_comp_op(f: int -> int -> bool)
   (stack: t_stack)(trace: t_trace)(vars: t_vars): com_var_interpreter =
   match stack with
   | Const(Integer i1) :: Const(Integer i2) :: stack ->
      (Const(Boolean(f i1 i2)) :: stack, trace, vars)
   | _ -> ([], "Panic" :: trace, vars)

let interpret_lt = interpret_comp_op( < )
let interpret_gt = interpret_comp_op( > )

let interpret_bind(stack: t_stack)(trace: t_trace)
                  (vars: t_vars): com_var_interpreter =
   match stack with
   | Const(Sym x) :: v :: stack -> 
      (stack, trace, (x, v) :: vars)
   | _ -> ([], "Panic" :: trace, vars)

exception Found of value
let interpret_lookup(stack: t_stack)(trace: t_trace)
                    (vars: t_vars): com_var_interpreter =
   match stack with
   | Const(Sym x) :: stack -> (
      try (
         let _ = list_foreach vars (fun var -> 
         let (n, v) = var in
         if n = x then raise (Found v) ) in
      ([], "Panic" :: trace, vars) ) (*name not in env*)
      with Found v -> (v :: stack, trace, vars)
   )
   | _ -> ([], "Panic" :: trace, vars)

let interpret_fun(funComs: coms)
                 (stack: t_stack)(trace: t_trace)(vars: t_vars)
                 : com_var_interpreter =
   match stack with
   | Const(Sym x) :: stack -> 
      let f: closure = (x, vars, funComs) in
      (Closure f :: stack, trace, vars)
   | _ -> ([], "Panic" :: trace, vars)

exception UhOh of t_stack
let rec interpret_call(stack: t_stack)(trace: t_trace)(vars: t_vars): com_var_interpreter =
   match stack with
   | Closure c :: a :: stack -> 
      let (name, cVars, commands) = c in
      let (stack, trace, funVars) = 
      interpret_coms(commands)(a :: Closure c :: stack)
                    (trace)((name, Closure c) :: cVars)
      in (stack, trace, vars)
   | _ -> ([], "Panic" :: trace, vars)

and interpret_return(stack: t_stack)(trace: t_trace)(vars: t_vars): com_var_interpreter = 
   match stack with
   | Closure c :: a :: stack -> (a :: stack, trace, vars)
   | _ -> ([], "Panic" :: trace, vars)

and interpret_ifelse (ifTrue: coms)(ifFalse: coms)
                     (stack: t_stack)(trace: t_trace)(vars: t_vars)
                     : com_var_interpreter =
   match stack with
   | Const(Boolean true) :: xs -> 
      interpret_coms ifTrue xs trace vars
   | Const(Boolean false) :: xs -> 
      interpret_coms ifFalse xs trace vars
   | _ -> ([], "Panic" :: trace, vars)

and interpret_coms(cmds: coms)(stack: t_stack)(trace: t_trace)(vars: t_vars): com_var_interpreter =
   let rec loop(cmds: coms)(stack: t_stack)(trace: t_trace)(vars: t_vars): com_var_interpreter =
      match cmds with
      | Coms(cmd :: cmds) -> (
         let new_stack_trace = 
            match cmd with
            | Push(cnst) -> interpret_push(cnst)
            | Pop -> interpret_pop
            | Swap  -> interpret_swap
            | Trace -> interpret_trace
            | Add -> interpret_add
            | Sub -> interpret_sub
            | Mul -> interpret_mul
            | Div -> interpret_div
            | And -> interpret_and
            | Or  -> interpret_or
            | Not -> interpret_not
            | Lt  -> interpret_lt
            | Gt  -> interpret_gt
            | IfElse(coms1, coms2) -> interpret_ifelse(coms1)(coms2)
            | Bind  -> interpret_bind
            | Lookup -> interpret_lookup
            | Fun(funComs) -> interpret_fun(funComs)
            | Call -> interpret_call
            | Return -> interpret_return
         in match new_stack_trace stack trace vars with
         | (stack, "Panic" :: trace, vars) -> (stack, "Panic" :: trace, vars)
         | (stack, trace, vars) -> loop(Coms cmds)(stack)(trace)(vars)  )
      | _ -> (stack, trace, vars)
      in loop(cmds)(stack)(trace)(vars)
   ;;
      

let interpret_program(cmds: coms): t_trace =
   let (_, trace, _) = interpret_coms(cmds)([])([])([]) in trace


let interp (s : string) : t_trace option = 
   match string_parse(prog_parser()) s with
   | Some(pgm, remains) -> (
      match trim remains with
      | [] -> Some (interpret_program (Coms pgm))
      | _ -> None  )
   | _ -> None
