type exp = 
  | Int of int
  | BinOp of (binop * exp * exp)


and binop =
  | Add
  | Minus
  | Mult

let add e1 e2 = BinOp(Add,e1,e2)
let sub e1 e2 = BinOp(Minus,e1,e2) 
let mul e1 e2 = BinOp(Mult,e1,e2) ;;
let int n = (Int n) ;;



let rec print = function
| Int c -> N2t.print_string "Int ("; print_int c;  N2t.print_string ")\n"
| BinOp (op,e1,e2) -> N2t.print_string "BinOp ("; print_op op;  N2t.print_string "," ; print e1 ; N2t.print_string "," ; print e2 ; N2t.print_string ")"

and print_op = function
| Add -> N2t.print_string "Add"
| Minus -> N2t.print_string "Minus"
| Mult -> N2t.print_string "Mult" ;;

let e = add (sub (mul (int 3) (int 4)) (int 5)) (int 6) in
print e

