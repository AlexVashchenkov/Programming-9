let l = [0;1;1;0;1;1];;

let n = 2;;

let rec deg_2 n = if n = 0 then 1 else 2 * (deg_2 (n-1));;

let rec encode q n = 
	match q with
 [] -> 0
|a :: b -> a * (deg_2 n) + (encode b (n+1));;
