#use "./../../../classlib/OCaml/MyOCaml.ml";;

(*

Please implement the interp function following the
specifications described in CS320_Fall_2023_Project-1.pdf

Notes:
1. You are only allowed to use library functions defined in MyOCaml.ml
   or ones you implement yourself.
2. You may NOT use OCaml standard library functions directly.

*)

type const = 
            | Integer of int
            | Boolean of bool
            | Unit of unit
type com = Push of const | Pop | Trace 
         | Add | Sub | Mul | Div
         | And | Or | Not
         | Lt | Gt
type coms = Coms of com list
;;

let const_to_string(cnst: const) =
   match cnst with
   | Integer(i) -> int_to_string i
   | Boolean(b) ->  boolean_to_string b
   | Unit(()) -> "Unit"

let const_parser(): const parser =
   let* _ = whitespaces in
   (let* _ = char '-' in 
    let* x = natural in 
    pure(Integer(-x))) <|>
   (let* x = natural in
    pure(Integer x)) <|>
   (let* t = keyword "True" in pure(Boolean true)) <|>
   (let* f = keyword "False" in pure(Boolean false)) <|>
   (let* _ = many (not_char ';') in pure(Unit ()))

let rec com_parser(): com parser =
   let* _ = whitespaces in
   (
      let* _ = keyword "Push" in
      let* v = const_parser() in
      let* _ = char ';' in
      pure(Push(v))
   ) <|>
   (let* _ = keyword "Pop;" in pure(Pop)) <|>
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
   fail

let coms_parser()  =
   many(com_parser())
;;

type com_interpreter = const list * string list

let interpret_push(stack: const list)(trace: string list)(cnst: const): com_interpreter =
   (cnst :: stack, trace)

let interpret_pop(stack: const list)(trace: string list): com_interpreter =
   match stack with
   | cnst :: stack -> (stack, trace)
   | [] -> (stack, "Panic" :: trace)

let interpret_trace(stack: const list)(trace: string list): com_interpreter =
   match stack with
   | cnst :: stack -> 
      (Unit(()) :: stack, const_to_string(cnst) :: trace)
   | [] -> (stack, "Panic" :: trace)

let interpret_arithmetic(f: int -> int -> int)(stack: const list)(trace: string list): com_interpreter =
   match stack with
   | Integer i1 :: Integer i2 :: stack -> (Integer(f i1 i2) :: stack, trace)
   | _ -> ([], "Panic" :: trace)

let interpret_add = interpret_arithmetic ( + )
let interpret_sub = interpret_arithmetic ( - )
let interpret_mul = interpret_arithmetic ( * )
let interpret_div(stack: const list)(trace: string list) = 
   try interpret_arithmetic ( / )(stack)(trace) with 
   | Division_by_zero -> ([], "Panic" :: trace)

let interpret_and(stack: const list)(trace: string list): com_interpreter =
   match stack with
   | Boolean a :: Boolean b :: stack ->
      (Boolean(a && b) :: stack, trace)
   | _ -> ([], "Panic" :: trace)

let interpret_or(stack: const list)(trace: string list): com_interpreter =
   match stack with
   | Boolean a :: Boolean b :: stack ->
      (Boolean(a || b) :: stack, trace)
   | _ -> ([], "Panic" :: trace)

let interpret_not(stack: const list)(trace: string list): com_interpreter =
   match stack with
   | Boolean a :: stack -> 
      (Boolean(not a) :: stack, trace)
   | _ -> ([], "Panic" :: trace)

let interpret_lt(stack: const list)(trace: string list): com_interpreter =
   match stack with
   | Integer i1 :: Integer i2 :: stack ->
      (Boolean(i1 < i2) :: stack, trace)
   | _ -> ([], "Panic" :: trace)

let interpret_gt(stack: const list)(trace: string list): com_interpreter = 
   match stack with
   | Integer i1 :: Integer i2 :: stack ->
      (Boolean(i1 > i2) :: stack, trace)
   | _ -> ([], "Panic" :: trace)

let interpret_program(cmds: coms) : string list option =
   let rec loop(cmds: coms)(stack: const list)(trace: string list): string list option =
      match cmds with
      | Coms(cmd :: cmds) -> (
         let new_stack_trace = 
            match cmd with
            | Push(cnst) -> interpret_push(stack)(trace)(cnst)
            | Pop -> interpret_pop(stack)(trace)
            | Trace -> interpret_trace(stack)(trace)
            | Add -> interpret_add(stack)(trace)
            | Sub -> interpret_sub(stack)(trace)
            | Mul -> interpret_mul(stack)(trace)
            | Div -> interpret_div(stack)(trace)
            | And -> interpret_and(stack)(trace)
            | Or  -> interpret_or(stack)(trace)
            | Not -> interpret_not(stack)(trace)
            | Lt  -> interpret_lt(stack)(trace)
            | Gt  -> interpret_gt(stack)(trace)
         in match new_stack_trace with
         | (stack, "Panic" :: trace) -> Some("Panic" :: trace)
         | (stack, trace) -> loop(Coms cmds)(stack)(trace)
      )
      | _ -> Some(trace)
   in loop(cmds)([])([])

let interp (s : string) : string list option = 
   match string_parse(coms_parser()) s with
   | Some(pgm, []) -> interpret_program (Coms pgm)
   | _ -> None
   