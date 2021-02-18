
type 'a t = 'a list

let rec length_aux len = function
    [] -> len
  | _::l -> length_aux (len + 1) l

let length l = length_aux 0 l

let cons a l = a::l

let hd = function
    [] -> failwith "hd"
  | a::_ -> a

let tl = function
    [] -> failwith "tl"
  | _::l -> l

let rec append l1 l2 = match l1 with 
  [] -> l2
| a::q -> a::(append q l2);;

let rec rev_append l1 l2 =
  match l1 with
    [] -> l2
  | a :: l -> rev_append l (a :: l2)

let rev l = rev_append l []

let rec map f = function
    [] -> []
  | a::l -> let r = f a in r :: map f l

let rec mapi i f = function
    [] -> []
  | a::l -> let r = f i a in r :: mapi (i + 1) f l

let mapi f l = mapi 0 f l

let rev_map f l =
  let rec rmap_f accu = function
    | [] -> accu
    | a::l -> rmap_f (f a :: accu) l
  in
  rmap_f [] l


let rec iter f = function
    [] -> ()
  | a::l -> f a; iter f l

let rec iteri i f = function
    [] -> ()
  | a::l -> f i a; iteri (i + 1) f l

let iteri f l = iteri 0 f l

let rec fold_left f accu l =
  match l with
    [] -> accu
  | a::l -> fold_left f (f accu a) l

let rec fold_right f l accu =
  match l with
    [] -> accu
  | a::l -> f a (fold_right f l accu)

let rec map2 f l1 l2 =
  match (l1, l2) with
    ([], []) -> []
  | (a1::l1, a2::l2) -> let r = f a1 a2 in r :: map2 f l1 l2
  | (_, _) -> invalid_arg "List.map2"

let rev_map2 f l1 l2 =
  let rec rmap2_f accu l1 l2 =
    match (l1, l2) with
    | ([], []) -> accu
    | (a1::l1, a2::l2) -> rmap2_f (f a1 a2 :: accu) l1 l2
    | (_, _) -> invalid_arg "List.rev_map2"
  in
  rmap2_f [] l1 l2

let rec iter2 f l1 l2 =
  match (l1, l2) with
    ([], []) -> ()
  | (a1::l1, a2::l2) -> f a1 a2; iter2 f l1 l2
  | (_, _) -> invalid_arg "List.iter2"

(* let rec fold_left2 f accu l1 l2 =
  match (l1, l2) with
    ([], []) -> accu
  | (a1::l1, a2::l2) -> fold_left2 f (f accu a1 a2) l1 l2
  | (_, _) -> invalid_arg "List.fold_left2"

let rec fold_right2 f l1 l2 accu =
  match (l1, l2) with
    ([], []) -> accu
  | (a1::l1, a2::l2) -> f a1 a2 (fold_right2 f l1 l2 accu)
  | (_, _) -> invalid_arg "List.fold_right2"

let rec for_all p = function
    [] -> true
  | a::l -> p a && for_all p l

let rec exists p = function
    [] -> false
  | a::l -> p a || exists p l

let rec for_all2 p l1 l2 =
  match (l1, l2) with
    ([], []) -> true
  | (a1::l1, a2::l2) -> p a1 a2 && for_all2 p l1 l2
  | (_, _) -> invalid_arg "List.for_all2"


*)
let l1 = [1;2;3;4;5] ;;
let l2 = [6;7;8;9;10] ;;

iter N2t.print_int (map ((+) 1) (append l1 l2));;

(*
let rec exists2 p l1 l2 =
  match (l1, l2) with
    ([], []) -> false
  | (a1::l1, a2::l2) -> p a1 a2 || exists2 p l1 l2
  | (_, _) -> invalid_arg "List.exists2"
*)