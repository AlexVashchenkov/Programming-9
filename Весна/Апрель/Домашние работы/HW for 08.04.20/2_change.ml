type lambda = Var of string | App of lambda * lambda | Abs of string * lambda;;

let expr = App ((Abs ("x", Var "x")), (App ((Abs ("y", Var "y")), (Var "z"))));;

let rec replace b ksi t = 
	match b with
 Var psi -> if psi = ksi then t else b
|App (p,q) -> (App (replace p ksi t, replace q ksi t))
|Abs (psi,p) -> if psi = ksi then b else (Abs (psi,(replace p ksi t)));;

replace (Abs ("x",(Var "y"))) "y" (Var "x");;
