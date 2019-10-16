open Array;;

let a = init 900 (fun x -> x + 100);;

let probability = 1. /. 900.;;

let rec count_sum a = 
	a / 100 + (a / 10) mod 10 + a mod 10;;

let a_new = Array.map (fun x -> count_sum x) a;;

let rec sum a_new n =
	if n = (length a_new) then 0. else
	(float_of_int a_new.(n)) *. probability +. (sum a_new (n+1));;
  
sum a_new 0;; 
