open Array;;

let a = [|31;28;31;30;31;30;31;31;30;31;30;31|];;

let probability = 1. /. 12.;;

let rec sum a_new n =
	if n = (length a_new) then 0. else
	(float_of_int a.(n)) *. probability +. (sum a_new (n+1));;
  
 


