open String;;

let s = "10$";;

let rec deg_3 n = 
	if n = 0 then 1 else 3 * (deg_3 (n-1));;

let rec to_int s n = 
	if n = (length s) then 0 else 
	if s.[n] = '$' 
	then (-1) * (deg_3 ((length s) - n - 1)) + (to_int s (n+1))
	else (int_of_string (make 1 s.[n]))  * (deg_3 ((length s) - n - 1)) + (to_int s (n+1));;