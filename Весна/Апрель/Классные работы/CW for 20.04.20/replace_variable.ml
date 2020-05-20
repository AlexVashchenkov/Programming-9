type lambda = Index of int | App of lambda * lambda | Abs of lambda;;

let rec replace_variable variable deBrownIndex = 
	match variable with
 Index x -> if x >= deBrownIndex then Index 9999999 else Index (x + deBrownIndex)
|App (l1,l2) -> App ((replace_variable l1 deBrownIndex),(replace_variable l2 deBrownIndex))
|Abs l -> Abs (replace_variable l (deBrownIndex + 1));;

replace_variable (App ((Index 3),(Abs (Index 5)))) 5;;
