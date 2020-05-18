type lambda = Index of int | App of lambda * lambda | Abs of lambda;;.

let rec find_variable l n = 
	match l with
 Index x -> if x = n then 1 else 0
|App (l1,l2) -> (find_variable l1 n) + (find_variable l2 n)
|Abs l_ -> (find_variable l_ (n+1));;

find_variable (App ((Index 3),(Abs (Index 4)))) 2;;
