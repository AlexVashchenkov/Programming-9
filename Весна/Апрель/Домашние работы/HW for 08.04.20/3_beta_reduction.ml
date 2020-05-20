(*type lambda = Var of string | App of lambda * lambda | Abs of string * lambda;;*)

type l = Var of string * int | App of l * l | Abs of string * l;;

let rec print_l x = 
	match x with
 Var (x,n) -> print_string x
|App (y,z) -> print_string "(";(print_l y);(print_string " ");(print_l z);print_string ")"
|Abs (s,x) -> print_string "(\\";print_string s;print_string ".";(print_l x);print_string ")";;

let str = "";;
let tree = App (Abs ("x", Var ("x",3)), Var ("y",5));;


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

let rec check_free str tree n = 
	if (in_list (List.map (fun (x,y) -> y) (find_index tree [])) n) then true else false;;

let rec replace b ksi t = 
	match b with
 Var (psi,n) -> if psi <> ksi then b else (match t with
	Var (l,m) -> Var (l,n)
       |App (l1,l2) -> t
       |Abs (s,x) -> t)

|App (p,q) -> (App (replace p ksi t, replace q ksi t))
|Abs (psi,p) -> if psi = ksi then b else (Abs (psi,(replace p ksi t)));;

let rec step term =
	match term with
 Var (s,n) -> Var (s,n)
|App (t1,t2) -> (match (t1,t2) with
	(Abs (s,x), t2) -> replace x s t2
	 |_ -> App (step t1, step t2))
|Abs (s,x) -> Abs (s, (step x));;

step tree;;
	
	 