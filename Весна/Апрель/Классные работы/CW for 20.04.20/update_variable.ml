type lambda = Index of int | App of lambda * lambda | Abs of lambda;;.

let rec update_variable variable deBrownIndex toUpdate= 
	match variable with
 Index x -> if x >= deBrownIndex then Index x else Index (toUpdate + deBrownIndex)
|App (l1,l2) -> App ((update_variable l1 deBrownIndex toUpdate),(update_variable l2 deBrownIndex toUpdate))
|Abs l -> Abs (update_variable l (deBrownIndex + 1) toUpdate);;

update_variable (App ((Index 3),(Abs (Index 5)))) 3 5;;
