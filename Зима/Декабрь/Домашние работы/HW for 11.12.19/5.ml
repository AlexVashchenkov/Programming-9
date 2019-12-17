let s = 54;;
let n = 2;;

let rec decode s n = 
	if s < 1 then [] else
	if s = 1 then [1] else
	[(s mod n)] @ (decode (s / n) n) ;; 