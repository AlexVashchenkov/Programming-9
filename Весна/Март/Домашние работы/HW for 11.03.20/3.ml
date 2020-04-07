type l = Var of string * int | App of l * l | Abs of string * l;;

let rec print_l x = 
	match x with
 Var (x,n) -> print_string x
|App (y,z) -> print_string "(";(print_l y);(print_string " ");(print_l z);print_string ")"
|Abs (s,x) -> print_string "(\\";print_string s;print_string ".";(print_l x);print_string ")";;

let str = "(x x)";;
let tree = App (Var ("x",1), Var ("x",3));;

let rec find_all tree = 
	match tree with
 Var (x,n) -> 1
|App (x,y) -> (find_all x) + (find_all y)
|Abs (s,x) -> 0;;

let rec in_list lst x = 
	match lst with
 [] -> false
|a :: b -> if a = x then true else (in_list b x);;

let rec find_index tree lst = 
	match tree with
 Var (x,n) -> if (in_list lst x) then [] else [(x,n)]
|App (x,y) -> (find_index x lst) @ (find_index y lst)
|Abs (s,x) -> (find_index x (s :: lst));;
