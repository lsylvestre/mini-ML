(*
let b = Bc.Push(Bc.Constant(17)) :: Bc.Push(Bc.Constant(42)) :: Bc.BinOp(Bc.Add) :: []

let _ = Bc_print.string_of_instrs (fun x -> print_string x) b
*)

let k = Kast.Constant (Kast.Bool(true))
let by = Kast2bc.bc_of_exp (k)


let _ = Bc_print.string_of_instrs (fun s -> Pervasives.print_string s) by

let _ = print_string "ok"
(*

let e = Kast.Let(0,Kast.If(Kast.BinOp(Ast.Le,Kast.Constant 17,Kast.Constant 42)))
let by = Kast2bc.bc_of_exp e
let _ =
        Bc_print.string_of_instrs (fun s -> Pervasives.print_string s) by

        *)

