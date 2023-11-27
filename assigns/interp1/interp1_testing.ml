#use "./assigns/interp1/MySolution/interp1.ml";;

let prog1 = "Push True; Push False; Push 69; Trace; Push -1872; Trace; Push hey man!; Trace;"
let () = assert(interp prog1 = Some(["Unit"; "-1872"; "69"]))

let prog2 = "Push True; Push 0; Push (); Trace; Pop; Trace; Pop; Trace;"
let () = assert(interp prog2 = Some(["True"; "0"; "Unit";]))

let prog3 = "Push 1; Push 2; Push 3; Push 4; Push 5; Add; Add; Add; Add;"
let () = assert(interp (prog3 ^ "Trace;") = Some(["15"]))
let () = assert(interp (prog3 ^ "Add; Trace;") = Some(["Panic"]))

let prog4 = "Push 1; Push 2; Add; Push 3; Lt; Push 4; Add;"
let () = assert(interp prog4 = Some(["Panic"]))

let prog5 = "Push 4; Pop; Add;"
let () = assert(interp prog5 = Some(["Panic"]))

let prog6 = "Push 0; Push 5; Div;"
let () = assert(interp prog6 = Some(["Panic"]))

let prog7 = "Push 5; Push 0; Div; Trace;"
let () = assert(interp prog7 = Some(["0"]))

let prog8 = "Push 2; Push 3; Gt; Push 4; Push 5; Gt; And; Trace;"
let () = assert(interp prog8 = Some(["True"]))

let prog9 = "
  Push 2; 
  Push 3; 
  Lt; 
  Push 5; 
  Push 4; 
  Lt; 
  Or;
  Trace;"
let () = assert(interp prog9 = Some(["True"]))

let prog10 = "
  Push 1; 
  Push True; 
  And; 
  Push 2; 
  Push 3; 
  Add; 
  Trace;"
let () = assert(interp prog10 = Some(["Panic"]))

let prog11 = "
  Push 1; 
  Push 2; 
  Gt; 
  Push 3; 
  Push 4; 
  Lt; 
  Not; 
  And; 
  Trace;"
let () = assert(interp prog11 = Some(["True"]))

let prog12 = "
  Push 1; Push 2; Push 3; Lt; Trace; Pop; Gt; Trace;
"
let () = assert(interp prog12 = Some(["Panic"; "False"]))

let () = print_endline("All tests passed!")