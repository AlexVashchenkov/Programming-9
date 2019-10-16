(*Матожидание старшей цифры от 1 до N*)

open Array;;

let n = read_int();;

let a = init n (fun x -> x+1);;

let probability = 1. /. (float_of_int n);;

let rec deg_10 x = 
	if x = 0 then 1 else 10 * (deg_10 (x-1));;

let rec closest a x = 
	if (deg_10 x) < a && (deg_10 (x+1)) > a then x else (closest a (x+1));;
	
let a_new = Array.init n (fun x -> a.(x) / (deg_10 (closest a.(x) 0)));;

let rec sum a_new x =
	if x = n then 0. else
	(float_of_int a_new.(x)) *. probability +. (sum a_new (x+1));;

print_float (sum a_new 0);;