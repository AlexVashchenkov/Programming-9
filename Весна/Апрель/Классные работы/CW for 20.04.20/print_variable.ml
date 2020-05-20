type lambda = Var of  | App of lambda * lambda | Abs of lambda;;.

let 	