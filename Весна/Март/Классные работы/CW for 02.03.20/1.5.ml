type l = Var of string | App of l * l | Abs of string * l;;

let rec print_l x = 
	match x with
 Var x -> print_string x
|App (y,z) -> print_string "(";(print_l y);(print_string " ");(print_l z);print_string ")"
|Abs (s,x) -> print_string "(\\";print_string s;print_string ".";(print_l x);print_string ")";;
