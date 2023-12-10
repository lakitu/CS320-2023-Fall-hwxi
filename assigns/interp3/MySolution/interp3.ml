(* ****** ****** *)
(*
 MyOCaml.ml
 is a class library
 built for CS320, Fall, 2023
*)
(* ****** ****** *)
exception False;;
(* ****** ****** *)
exception Subscript;;
(* ****** ****** *)

(** Return the character with the given ASCII code. **)
let chr = Char.chr;;

(** return the ASCII code of the argument **)
let ord = Char.code;;

(** make n c is a string of length n with each index holding the character c **)
let str(c0) = String.make 1 c0;;

(** checks if a character is lowercase **)
let char_islower(ch: char) = (ch >= 'a' && ch <= 'z');;

(** checks if a character is uppercase **)
let char_isupper(ch: char) = (ch >= 'A' && ch <= 'Z');;

(** checks if a character is a digit **)
let char_isdigit(ch: char) = (ch >= '0' && ch <= '9');;

(** checks if a character is a alphabetical letter **)
let char_isletter(ch: char) =
  (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z');;

(** checks if a character is a alphabetical or numerical letter **)
let char_isalphanum(ch: char) =
  char_islower(ch) || char_isupper(ch) || char_isdigit(ch);;

(** checks if a character is a whitespace character **)
let char_iswhitespace(ch: char) =
  (ch = ' ' || ch = '\n' || ch = '\r' || ch = '\t');;

(** converts a character to lowercase if applicable **)
let char_tolower(ch: char) =
  if char_isupper(ch) then chr(ord(ch) - ord('A') + ord('a')) else ch
;;

(** converts a character to uppercase if applicable **)
let char_toupper(ch: char) =
  if char_islower(ch) then chr(ord(ch) - ord('a') + ord('A')) else ch
;;

(** converts int digit to a character **)
let char_of_digit (d0: int): char =
  let () = assert(d0 >= 0) in
    let () = assert(d0 <= 9) in
      chr(ord('0') + d0)
;;(* end of [char_of_digit] *)


(** converts character to a digit**)
let digit_of_char(ch: char): int =
  let () = assert(ch >= '0') in
    let () = assert(ch <= '9') in
      ord(ch) - ord('0')
;;(* end of [digit_of_char] *)

(* ****** TYPE ANNOTATIONS ****** *)

(** takes a collection 'xs and a predicate function ('x0 -> bool), and it returns a boolean value if all satisfy or not **)
type ('xs, 'x0) forall = 'xs -> ('x0 -> bool) -> bool

(** takes a generic collection 'xs and applies a function ('x0 -> unit) which may produce a side effect **)
type ('xs, 'x0) foreach = 'xs -> ('x0 -> unit) -> unit

(** takes a generic collection 'xs and applies a function ('x0 -> unit) which may produce a side effect in reverse order **)
type ('xs, 'x0) rforeach = 'xs -> ('x0 -> unit) -> unit

(** takes a generic type 'xs and returns a list of values of type 'x0 **)
type ('xs, 'x0) listize = 'xs -> 'x0 list

(** takes a generic type 'xs and returns an array of values of type 'x0 **)
type ('xs, 'x0) arrnize = 'xs -> 'x0 array

(** takes a generic type 'xs and returns a list of values of type 'x0 in reverse order **)
type ('xs, 'x0) rlistize = 'xs -> 'x0 list

(** takes a generic type 'xs and returns an array of values of type 'x0 in reverse order **)
type ('xs, 'x0) rarrnize = 'xs -> 'x0 array

(** takes a collection 'xs and a function ('x0 -> 'y0) for transforming elements of type 'x0 into elements of type 'y0,
    and it returns a list of the transformed elements. **)
type ('xs, 'x0, 'y0) map_list = 'xs -> ('x0 -> 'y0) -> 'y0 list

(** takes a collection 'xs and a function ('x0 -> 'y0) for transforming elements of type 'x0 into elements of type 'y0,
    and it returns a list of the transformed elements in reverse order. **)
type ('xs, 'x0, 'y0) map_rlist = 'xs -> ('x0 -> 'y0) -> 'y0 list

type ('xs, 'x0, 'r0) foldleft = 'xs -> 'r0 -> ('r0 -> 'x0 -> 'r0) -> 'r0

type ('xs, 'x0, 'r0) foldright = 'xs -> 'r0 -> ('x0 -> 'r0 -> 'r0) -> 'r0

(* ****** ****** *)

(** run a work() fxn n0 times starting from 0 and ending at n0 **)
let int1_foreach (n0:int) (work: int -> unit): unit =
  for i0 = 0 to n0-1 do work(i0) done
;;

(** run a work() fxn n0 times starting from n0 and ending at 0 **)
let int1_rforeach (n0:int) (work: int -> unit): unit =
  for i0 = 0 to n0-1 do work(n0-1-i0) done
;;

(** init n f is a string of length n with index i holding the character f i (called in increasing index order). **)
let string_init = String.init;;

(** length s is the length (number of bytes/characters) of s **)
let string_length = String.length;;

(** read the contents of file into a string **)
let string_of_file(path: string) =
  let fp = open_in path in
  let rec loop () =
    match input_line fp with
    | s -> s ^ "\n" ^ (loop ())
    | exception End_of_file -> ""
  in
  let res = loop () in
  let () = close_in fp in
  res

(** get_at s i is the character at index i in s. This is the same as writing s.[i] **)
let string_get_at(cs:string)(i0:int): char = String.get cs i0;;

(* ****** ****** *)

(** get the first char *)
let string_head(cs:string):char = string_get_at(cs)(0)
(** get the rest of chars *)
let string_tail(cs) =
string_init(string_length(cs)-1)(fun i -> string_get_at(cs)(i+1))

(* ****** ****** *)

(** create a string c0+cs **)
let string_cons(c0: char)(cs: string): string =
  string_init(string_length(cs) + 1)(
    fun i -> if i <= 0 then c0 else string_get_at cs (i-1)
  )
;;

(** create a string cs+c0 **)
let string_snoc(cs: string)(c0: char): string =
  let len = string_length(cs) in
    string_init(len + 1)(
      fun i -> if i < len then string_get_at (cs) (i) else c0
    )
;;

(** turn cs into an uppercase string **)
let string_toupper(cs: string): string =
  string_init(string_length(cs))(
    fun i0 -> char_toupper(string_get_at(cs)(i0))
  )
;;

(** turn cs into a lowercase string **)
let string_tolower(cs: string): string =
  string_init(string_length(cs))(
    fun i0 -> char_tolower(string_get_at(cs)(i0))
  )
;;

(** for each character of string cs from 0 to length cs, apply the work function **)
let string_foreach(cs: string)(work: char -> unit) =
  int1_foreach(string_length(cs))(
    fun i0 -> work(string_get_at(cs)(i0))
  )
;;

(** for each character of string cs from length cs to 0, apply the work function **)
let string_rforeach(cs: string)(work: char -> unit) =
  int1_rforeach(string_length(cs))(
    fun i0 -> work(string_get_at(cs)(i0))
  )
;;

(** init n f is a string of length n with index i holding the character f i (called in increasing index order) **)
let string_tabulate = String.init;;

(** returns a fresh array containing the elements of xs which is a list of generic a **)
let list_arrnize(xs: 'a list): 'a array = Array.of_list(xs)

(** adds the reverse of generic list xs to generic list ys: xs=[1;2;3] ys=[4;5;6] = [3;2;1;4;5;6] **)
let rec list_revapp(xs: 'a list)(ys: 'a list): 'a list =
  match xs with
  | [] -> ys
  | x1 :: xs -> list_revapp(xs)(x1 :: ys)
;;

(** reverses a generic list **)
let list_reverse(xs: 'a list): 'a list = list_revapp(xs)([]);;

(** iterate through all elements in generic list xs and run the test function on each element. If all pass then return true **)
let rec list_forall(xs: 'a list)(test: 'a -> bool): bool =
  (
    match xs with
    | [] -> true
    | x1 :: xs -> (
        test(x1) && list_forall(xs)(test)
      )
  )
;;

(** iterate through all elements in generic list xs and run the test fxn on eahc element. If one passes then return true **)
let rec list_exists(xs: 'a list)(test: 'a -> bool): bool =
  (
    match xs with
    | [] -> false
    | x1 :: xs -> (
        test(x1) || list_exists(xs)(test)
      )
  )
;;

(** iterate through each element of generic list and apply the work function to that element **)
let rec list_foreach(xs: 'a list) (work: 'a -> unit): unit =
  (
    match xs with
    | [] -> ()
    | x1 :: xs -> (
        work(x1); list_foreach(xs)(work)
      )
  )
;;

(** iterate through each element of generic list and apply the work function to that element in reverse order **)
let rec list_rforeach(xs: 'a list) (work: 'a -> unit): unit =
  list_foreach(list_reverse(xs))(work)
;;

(** the forall_to_foreach function takes a forall function and converts it into a foreach function that applies a
    given action to each element in the collection while ensuring that all elements are processed **)
let forall_to_foreach(forall: ('xs, 'x0) forall): ('xs, 'x0) foreach =
  fun(xs)(work) -> let _ = forall(xs)(fun(x0) -> (work(x0); true)) in ()
;;

(** **)
let foreach_to_forall(foreach: ('xs, 'x0) foreach): ('xs, 'x0) forall =
  fun(xs)(test) ->
    try
      let() = foreach(xs)(fun(x0) -> if test(x0) then () else raise False)
    in( true ) with False(*void*) -> (false)
;;(* end of [foreach_to_forall]: let *)

(** **)
let foreach_to_foldleft(foreach: ('xs, 'x0) foreach): 'xs -> 'r0 -> ('r0 -> 'x0 -> 'r0) -> 'r0 =
  fun(xs)(r0)(fopr) ->
    let res = ref(r0) in
      foreach(xs)(fun(x0) -> res := fopr(!res)(x0));
    !res
;;(* end of [foreach_to_foldleft]: let *)

(** **)
let rec
foreach_to_map_list(foreach: ('xs, 'x0) foreach): ('xs, 'x0, 'y0) map_list =
fun(xs)(fopr) ->
list_reverse(foreach_to_map_rlist(foreach)(xs)(fopr))
and
foreach_to_map_rlist(foreach: ('xs, 'x0) foreach): ('xs, 'x0, 'y0) map_rlist =
fun(xs)(fopr) ->
let res = ref([]) in
foreach(xs)(fun(x0) -> res := fopr(x0) :: !res); !res
;;(* end of [foreach_to_map_rlist]: let *)

(** **)
let rec foreach_to_listize(foreach: ('xs, 'x0) foreach) : ('xs, 'x0) listize =
  fun(xs) -> foreach_to_map_list(foreach)(xs)(fun x -> x)
;;

(** **)
let rec foreach_to_rlistize(foreach: ('xs, 'x0) foreach) : ('xs, 'x0) rlistize =
  fun(xs) -> foreach_to_map_rlist(foreach)(xs)(fun x -> x)
;;

(** **)
let rec foreach_to_arrnize(foreach: ('xs, 'x0) foreach) : ('xs, 'x0) arrnize =
  (
    fun xs -> list_arrnize(foreach_to_listize(foreach)(xs))
  )
;;

(** **)
let rec foreach_to_rarrnize(foreach: ('xs, 'x0) foreach) : ('xs, 'x0) rarrnize =
  (
    fun xs -> list_arrnize(foreach_to_rlistize(foreach)(xs))
  )
;;

(** **)
let rec foreach_to_length(foreach: ('xs, 'x0) foreach): 'xs -> int =
  foldleft_to_length(foreach_to_foldleft(foreach))
  and
  foldleft_to_length(foldleft: ('xs,'x0,'r0) foldleft): 'xs -> int =
  (
    fun(xs) -> foldleft(xs)(0)(fun(r0)(x0) -> r0+1)
  )
;;

(** **)
let rforeach_to_foldright(rforeach: ('xs, 'x0) rforeach): ('xs, 'x0, 'r0) foldright =
  fun(xs)(r0)(fopr) ->
    let res = ref(r0) in
      rforeach(xs) (fun(x0) -> res := fopr(x0)(!res));
    !res
;;(* end of [rforeach_to_foldright]: let *)

(** **)
let int1_forall(n0) =
  foreach_to_forall(int1_foreach)(n0)
;;

(** **)
let string_forall(cs) =
  foreach_to_forall(string_foreach)(cs)
;;

(** **)
let int1_listize(n0) =
  foreach_to_listize(int1_foreach)(n0)
;;
let int1_rlistize(n0) =
  foreach_to_rlistize(int1_foreach)(n0)
;;

(** **)
let string_listize(cs) =
  foreach_to_listize(string_foreach)(cs)
;;

(** **)
let string_rlistize(cs) =
  foreach_to_rlistize(string_foreach)(cs)
;;

(** **)
let int1_foldleft(n0) =
  foreach_to_foldleft(int1_foreach)(n0)
;;

(** **)
let list_foldleft(xs) =
  foreach_to_foldleft(list_foreach)(xs)
;;

(** **)
let string_foldleft(cs) =
  foreach_to_foldleft(string_foreach)(cs)
;;

(** **)
let int1_foldright(n0) =
  rforeach_to_foldright(int1_rforeach)(n0)
;;

(** **)
let list_foldright(xs) =
  rforeach_to_foldright(list_rforeach)(xs)
;;

(** **)
let string_foldright(cs) =
  rforeach_to_foldright(string_rforeach)(cs)
;;
(* ****** ****** *)

(*
  let foreach_to_foldright(foreach: ('xs, 'x0) foreach): 'xs -> 'r0 -> ('x0 -> 'r0 -> 'r0) -> 'r0 =
    fun xs r0 fopr ->
      let xs = foreach_to_rlistize(foreach)(xs) in
        list_foldleft(xs)(r0)(fun r0 x0 -> fopr x0 r0)
*)

(* ****** ****** *)

(** transforms the work done by fwork into a list. **)
let list_make_fwork(fwork: ('x0 -> unit) -> unit): 'x0 list =
  let res = ref([]) in
    let work(x0) = (res := (x0 :: !res))
    in(*let*)(fwork(work); list_reverse(!res) )
;;

let list_make_filter(test: 'x0 -> bool)(fwork: ('x0 -> unit) -> unit): 'x0 list =
  let res = ref([]) in
    let work(x0) =
      if test(x0) then (res := (x0 :: !res))
      in(*let*) (fwork(work); list_reverse(!res))
;;

(* ****** ****** *)

(** transforms the work done by fwork into a list in reverse order. **)
let list_rmake_fwork(fwork: ('x0 -> unit) -> unit): 'x0 list =
  let res = ref([]) in
    let work(x0) = (res := (x0 :: !res)) in (fwork(work); !res)
;;

let list_rmake_filter(test: 'x0 -> bool)(fwork: ('x0 -> unit) -> unit): 'x0 list =
  let res = ref([]) in
    let work(x0) = if test(x0) then (res := (x0 :: !res)) in (fwork(work); !res)
;;

(* ****** ****** *)

(** The result of the entire expression is a string that represents the characters processed by the fwork function **)
let string_make_fwork(fwork: (char -> unit) -> unit): string =
  let xs =
    Array.of_list(list_make_fwork(fwork))
  in String.init (Array.length(xs)) (fun i -> xs.(i))
;;

(** **)
let string_rmake_fwork(fwork: (char -> unit) -> unit): string =
  let xs =
    Array.of_list(list_rmake_fwork(fwork))
  in String.init (Array.length(xs)) (fun i -> xs.(i))
;;

(* ****** ****** *)

(** appends two lists together: [1;2;3] [4;5;6] = [1;2;3;4;5;6]**)
let list_append(xs: 'a list)(ys: 'a list): 'a list =
  list_make_fwork(
    fun work -> (list_foreach xs work; list_foreach ys work)
  )
;;

(** takes in a list of lists and returns one list with all the lists concatenated: [[1;2] ; [3;4]] = [1;2;3;4] **)
let list_concat(xss: 'a list list): 'a list =
  list_make_fwork(
    fun work -> list_foreach xss (fun xs -> list_foreach xs work)
  )
;;

(* ****** ****** *)

let string_filter
(cs: string)(test: char -> bool) =
string_make_fwork
(fun work -> string_foreach(cs)(fun c -> if test(c) then work(c)))
;;

(* ****** ****** *)

let string_append(xs: string)(ys: string): string =
  string_make_fwork(
    fun work -> (string_foreach xs work; string_foreach ys work)
  )
;;

(** takes a list of strings and gives a string with the strings concatenated **)
let string_concat_list(css: string list): string =
  string_make_fwork(
    fun work -> list_foreach css (fun cs -> string_foreach cs work)
  )
;;

(* ****** ****** *)

type 'a strcon =
  StrNil
| StrCons of
  'a * (unit -> 'a strcon)

(* ****** ****** *)

type 'a stream =
unit -> 'a strcon (* thunk *)

(* ****** ****** *)

let rec
stream_map
(fxs: 'a stream)
(fopr: 'a -> 'b): 'b stream =
fun () ->
match fxs() with
|
StrNil -> StrNil
|
StrCons(x1, fxs) ->
StrCons
(fopr(x1), stream_map(fxs)(fopr))
;;
(* ****** ****** *)

let rec
stream_foreach
(fxs: 'a stream)
(work: 'a -> unit): unit =
match fxs() with
| StrNil -> ()
| StrCons(x1, fxs) ->
  (work(x1); stream_foreach(fxs)(work))
;;
(* ****** ****** *)

let
int1_map_stream
(n0: int)
(fopr: int -> 'a): 'a stream =
let rec
helper(i: int) =
fun () ->
if i >= n0
then StrNil(*void*)
else StrCons(fopr(i), helper(i+1)) in helper(0)
;;
(* ****** ****** *)

let rec
stream_append
(fxs: 'a stream)
(fys: 'a stream): 'a stream = fun() ->
match fxs() with
| StrNil -> fys()
| StrCons(x1, fxs) ->
  StrCons(x1, stream_append(fxs)(fys))
;;
(* ****** ****** *)

let rec
stream_concat_list
(fxss: 'a stream list): 'a stream = fun() ->
match fxss with
| [] -> StrNil
| fxs1 :: fxss -> stream_append(fxs1)(stream_concat_list(fxss))()
;;
(* ****** ****** *)

(** open the option and apply the work function to that element **)
let option_foreach(o0: 'a option)(work: 'a -> unit): unit =
  match o0 with
  | Some(a) -> work(a)
  | None    -> ()

(** open the option and run the test function on each element. If all pass then return true **)
let option_forall(o0) =
  foreach_to_forall(option_foreach)(o0)

let option_foldleft(o: 'a option)(acc: 'r0)(fopr: 'r0 -> 'a -> 'r0): 'r0 =
  foreach_to_foldleft(option_foreach)(o)(acc)(fopr)

let option_listize(xs: 'xs): 'x0 list =
  foreach_to_listize(option_foreach)(xs)

let option_map(o: 'a option)(fopr: 'a -> 'b): 'b option =
  match o with
  | Some(a) -> Some(fopr(a))
  | None    -> None

let option_bind(o: 'a option)(fopr: 'a -> 'b option): 'b option =
  option_foldleft(o)(None)(fun _ x -> fopr(x))

let option_cond(c0: bool)(v0: unit -> 'a): 'a option =
  if c0 then Some(v0()) else None

(* ****** ****** *)

let (let@) = option_bind

(* ****** ****** *)

type 'a parser = char list -> ('a * char list) option

let string_parse(p: 'a parser)(s: string): ('a * char list) option =
  p(string_listize(s))

let pure(a: 'a) =
  fun xs -> Some(a, xs)

let fail: 'a parser =
  fun _ -> None

let bind(p: 'a parser)(q: 'a -> 'b parser) =
  fun xs ->
    let@ (a, xs) = p(xs) in
    q(a)(xs)

let read: char parser =
  fun xs ->
  match xs with
  | x :: xs -> Some (x, xs)
  | _ -> None

let satisfy(f: char -> bool) =
  fun xs ->
  match xs with
  | x :: xs -> option_cond(f(x))(fun () -> x, xs)
  | _ -> None

let char(c: char) =
  satisfy((=) c)

let seqright(p1: 'a parser)(p2: 'b parser) =
  fun xs ->
  let@ (_, xs) = p1(xs) in
  p2 xs

let seqleft(p1 : 'a parser)(p2 : 'b parser) =
  fun xs ->
  let@ (x, xs) = p1(xs) in
  let@ (_, xs) = p2(xs) in
  Some (x, xs)

let disj(p1 : 'a parser)(p2 : 'a parser) =
  fun xs ->
  option_foldleft(p1(xs))(fun () -> p2(xs))(fun _ x () -> Some x)()

let map(p : 'a parser)(f : 'a -> 'b) =
  fun xs ->
  option_map(p(xs))(fun (a, xs) -> f(a), xs)

let rec many(p : 'a parser) =
  fun ls ->
  match p(ls) with
  | Some (x, ls) ->
    (match many(p)(ls) with
     | Some (xs, ls) -> Some (x :: xs, ls)
     | None -> Some (x :: [], ls))
  | None -> Some ([], ls)

let rec many1(p: 'a parser) =
  fun ls ->
  match p(ls) with
  | Some (x, ls) ->
    (match many(p)(ls) with
     | Some (xs, ls) -> Some (x :: xs, ls)
     | None -> Some (x :: [], ls))
  | None -> None

let rec many'(p: unit -> 'a parser) =
  fun ls ->
  match p(())(ls) with
  | Some (x, ls) ->
    (match many'(p)(ls) with
     | Some (xs, ls) -> Some (x :: xs, ls)
     | None -> Some (x :: [], ls))
  | None -> Some ([], ls)

let rec many1'(p : unit -> 'a parser) =
  fun ls ->
  match p(())(ls) with
  | Some (x, ls) ->
    (match many'(p)(ls) with
     | Some (xs, ls) -> Some (x :: xs, ls)
     | None -> Some (x :: [], ls))
  | None -> None

let whitespace =
  fun xs ->
  match xs with
  | c :: xs ->
    option_cond(char_iswhitespace(c))(fun () -> (), xs)
  | _ -> None

let digit =
  satisfy char_isdigit

let natural : int parser =
  fun ls ->
  let@ (xs, ls) = many1 digit ls in
  Some(list_foldleft(xs)(0) (fun acc n -> acc * 10 + digit_of_char(n)), ls)

let literal(s: string) =
  fun ls ->
  let cs = string_listize s in
  let rec loop cs ls =
    match cs, ls with
    | [], _ -> Some ((), ls)
    | c :: cs, x :: xs ->
      if x = c
      then loop cs xs
      else None
    | _ -> None
  in loop cs ls

(* ****** ****** *)

let (>>=)  = bind
let (let*) = bind
let (>>)   = seqright
let (<<)   = seqleft
let (<|>)  = disj
let (>|=)  = map
let (>|)   = fun p c -> map p (fun _ -> c)

(* ****** ****** *)

let whitespaces =
  (many whitespace) >| ()

let whitespaces1 =
  (many1 whitespace) >| ()

let keyword(s: string) =
  (literal s) >> whitespaces >| ()

(* end of [CS320-2023-Fall-classlib-MyOCaml.ml] *)

let string_constructor(cs: char list): string =
  string_make_fwork(fun work -> list_foreach cs work)

let reverse_list(xs: 'a list): 'a list =
  list_foldleft(xs)([])(fun r0 x0 -> x0 :: r0)

let int_to_string(num: int): string =
  let rec loop(num: int)(acc: char list): string =
    if num = 0 then string_make_fwork(fun work -> list_foreach(acc)(work))
    else (
      let digit = num mod 10 in
      let chr = char_of_digit(digit) in
      loop(num/10)(chr :: acc) )
  in 
  if num = 0 then "0"
  else if num < 0 then "-" ^ loop(-num)([])
  else loop(num)([])

let boolean_to_string(b: bool): string =
  match b with
  | true -> "True"
  | false -> "False"

let explode(s: string) = string_listize(s)

let list_tail(xs: 'a list): 'a list =
  match xs with
  | x0 :: xs -> xs
  | [] -> []

let string_remove_tail(str: string): string = 
  str |> string_listize |> reverse_list |> list_tail |> reverse_list |> string_constructor

let not_char(c: char) = 
  satisfy(fun c0 -> not ((=) c c0))

let word_until(c: char): string parser =
  fun xs -> 
    match many(not_char ';')(xs) with
    | Some(arr, rest) -> Some(string_constructor arr, rest)
    | None -> None

  (* remove blank chars at the front of a list *)
let rec trim cs =
  match cs with
  | [] -> cs
  | '\n' :: cs -> trim cs
  | '\t' :: cs -> trim cs
  | '\r' :: cs -> trim cs
  | ' ' :: cs -> trim cs
  | _ -> cs
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
and coms = com list
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
   (  let* _ = literal "Push" in
      let* _ = whitespaces1 in
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
   (  let* _ = literal "If" in
      let* _ = whitespaces1 in
      let* coms1 = many (com_parser()) in 
      let* _ = whitespaces in
      let* _ = literal "Else" in
      let* _ = whitespaces1 in
      let* coms2 = many (com_parser()) in 
      let* _ = whitespaces in
      let* _ = keyword "End;" in 
      pure(IfElse(coms1, coms2))  ) <|>
   (let* _ = keyword "Bind;" in pure(Bind)) <|>
   (let* _ = keyword "Lookup;" in pure(Lookup)) <|>
   (  let* _ = literal "Fun" in
      let* _ = whitespaces1 in
      let* func = many (com_parser()) in
      let* _ = whitespaces in
      let* _ = keyword "End;" in
      pure(Fun func)  ) <|>
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
exception UhOh of t_stack

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
let closure_to_string(c: closure) =
  let (name, _, _) = c in "Fun<"^name^">"
let interpret_trace(stack: t_stack)(trace: t_trace)(vars: t_vars): com_var_interpreter =
  match stack with
  | Const cnst :: stack -> 
    (Const(Unit ()) :: stack, const_to_string(cnst) :: trace, vars)
  | Closure c :: stack ->
    (Const(Unit ()) :: stack, closure_to_string(c) :: trace, vars)
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
         if n = x then raise (Found v)  ) in
      ([], "Panic" :: trace, vars) ) (*name not in env*)
      with Found v -> (v :: stack, trace, vars)  )
   | _ -> ([], "Panic" :: trace, vars)

let interpret_fun(funComs: coms)
                 (stack: t_stack)(trace: t_trace)(vars: t_vars)
                 : com_var_interpreter =
   match stack with
   | Const(Sym x) :: stack -> 
      let f: closure = (x, vars, funComs) in
      (Closure f :: stack, trace, vars)
   | _ -> ([], "Panic" :: trace, vars)

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
      | cmd :: cmds -> (
         let com_interp = 
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
         in match com_interp stack trace vars with
         | (stack, "Panic" :: trace, vars) -> (stack, "Panic" :: trace, vars)
         | (stack, trace, vars) -> loop(cmds)(stack)(trace)(vars)  )
      | _ -> (stack, trace, vars)
      in loop(cmds)(stack)(trace)(vars)
   ;;
      

let interpret_program(cmds: coms): t_trace =
   let (_, trace, vars) = interpret_coms(cmds)([])([])([]) 
   in trace


let interp (s : string) : t_trace option = 
   match string_parse(prog_parser()) s with
   | Some(pgm, remains) -> (
      match trim remains with
      | [] -> Some (interpret_program pgm)
      | _ -> None  )
   | _ -> None
;;
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