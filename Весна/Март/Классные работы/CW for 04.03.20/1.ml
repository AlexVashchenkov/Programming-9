type l = Var of string | App of l * l | Abs of string * l;;

let rec create_l x = 
	match x with
 Var x -> x
|App (y,z) -> "(" ^ (create_l y) ^ " " ^ (create_l z) ^ ")"
|Abs (s,x) -> "(\\" ^ s ^ "." ^ (create_l x) ^ ")";;


let rec input_var b ksi t = 
	match b with
 Var psi -> if psi = ksi then t else b
|App (p,q) -> (App (input_var p ksi t, input_var q ksi t))
|Abs (psi,p) -> if psi = ksi then b else (Abs (psi,(input_var p ksi t)));;

let rec make_innocent expr = 
	match expr with
 Var s -> s
|App (y,z) -> (make_innocent y) ^ (make_innocent z)
|Abs (x,s) -> x ^ (make_innocent s);;
 