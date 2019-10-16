let letters = ["a";"b";"c";"d";"e"];;
let varieties = [ 1. /. 16. ; 1. /. 8. ; 1. /. 4. ; 1. /. 2. ; 1./.16.];;

let rec shennon l = 
	match l with
 [] -> 0.
|a :: b -> (-. (a *. (log a) /. (log 2.))) +. (shennon b);;

let rec combine l1 l2 = 
	match (l1,l2) with
 ([],[]) -> []
|(a :: b,[]) -> failwith"combine"
|([],a :: b) -> failwith"combine"
|(a :: b,c :: d) -> (combine b d) @ [(a,c)];;

(combine letters varieties);;
