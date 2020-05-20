let k = read_int();;
let n = read_int();;

let rec power n = 
    if n = 0 then 1 
    else (2 * power (n-1));;

let rec binary_list k l = 
	if k <= 0 then l 
	else if ((k-((k/2)*2)) = 1) then (binary_list (k/2) (true::l))
	else (binary_list (k/2) (false::l));;

let rec add_zero l n = 
	if  List.length l > n then []
	else if List.length l = n then l
	else add_zero (false::l) n;;
 
let rec binary_int n m = 
	if n <= 0 then m 
	else if ((n-((n /2)*2)) = 1) then (binary_int (n/2) ((m*10)+1))
	else (binary_int (n/2) (m*10));;

let rec final_print_bool_list l n = 
    match l with
    | hd::tl -> if (hd = true) then (print_int ((binary_int n 1)/10); print_string" -> true \n"; final_print_bool_list tl (n+1))
	            else (print_int ((binary_int n 1)/10); print_string" -> false \n"; final_print_bool_list tl (n+1))
    | _ -> None;;

final_print_bool_list (add_zero (binary_list k []) (power n)) 0 ;;
