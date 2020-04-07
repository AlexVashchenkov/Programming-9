type l = Var of string | App of l * l | Abs of string * l;;

let rec create_l x = 
	match x with
 Var x -> x
|App (y,z) -> "(" ^ (create_l y) ^ " " ^ (create_l z) ^ ")"
|Abs (s,x) -> "(\\" ^ s ^ "." ^ (create_l x) ^ ")";;


let rec input_var b ksi t = 
	match b with
 Var psi -> if ksi = var then t else b
|App (p,q) -> (App (input_var p ksi t) (input_var q ksi t))

 