type l = Var of string | App of l * l | Abs of string * l;;

let rec print_l x = 
	match x with
 Var x -> print_string x
|App (y,z) -> print_string "(";(print_l y);(print_string " ");(print_l z);print_string ")"
|Abs (s,x) -> print_string "(\\";print_string s;print_string ".";(print_l x);print_string ")";;

let rec make_innocent expr = 
	match expr with
 Var s -> s
|App (y,z) -> (make_innocent y) ^ (make_innocent z)
|Abs (x,s) -> x ^ (make_innocent s);;

let rec alpha_equiv a b = 
	match (a,b) with
 (Var x, Var y) -> if x = y then true else false
|(App (w,x),App (y,z)) -> (alpha_equiv w y) && (alpha_equiv x z)
|(Abs (s1,l1),Abs (s2,l2)) -> alpha_equiv ;;
