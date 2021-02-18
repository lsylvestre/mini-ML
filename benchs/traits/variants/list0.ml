let f x = x :: []

let _ = match f 42 with 
        | [x] -> print_int x
        | _ -> ()