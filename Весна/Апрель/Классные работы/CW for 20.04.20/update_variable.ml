type lambda = Index of int | App of lambda * lambda | Abs of lambda;;.

let rec update_variable variable deBrownIndex = 
	if variable >= deBrownIndex then variable else variable + deBrownIndex   
replace_variable (App ((Index 3),(Abs (Index 5)))) 3 999;;
