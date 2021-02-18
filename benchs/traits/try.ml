
exception E

let _ = try raise E with E -> N2t.print_int 42

(* problème, le try se compile en un code avec PUSHGETGLOBAL *)