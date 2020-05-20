type lambda = Var of string | App of lambda * lambda | Abs of string * lambda;;

let expr = App ((Abs ("x", Var "x")), (App ((Abs ("y", Var "y")), (Var "z"))));;

let rec check_redex l =                                                      
	match l with
 App (x,y) -> match x with
 Var a -> false
 |App (a,b) -> false
 |Abs (a,b) -> true;;

let rec find_all_redex l = 
	match l with
 Var x -> []
|App (x,y) -> if (check_redex (App (x,y))) then [App (x,y)] @ (find_all_redex x) @ (find_all_redex y) else []
|Abs (x,y) -> (find_all_redex y);;


