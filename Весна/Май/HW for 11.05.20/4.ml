let k = read_int();;
let n = read_int();;

let rec power n = if n = 0 then 1 else (2 * power (n-1));;

let rec binary_list k l = 
	if k <= 0 then l 
	else if ((k-((k/2)*2)) = 1) then (binary_list (k/2) (true::l))
	else (binary_list (k/2) (false::l));;

let rec add_zero l n = 
	if  List.length l > n then []
	else if List.length l = n then l
	else add_zero (false::l) n;;
 
let rec print_bool_list l = 
    match l with
    | [hd] -> if hd = true then print_string "true]"
	            else print_string "false]"
    | hd::tl -> if hd = true then (print_string "true," ; print_bool_list tl)
	            else (print_string "false," ; print_bool_list tl)
    | _ -> print_string "]\n";;

Printf.printf "f(%i,%i) = [" k n;;
print_bool_list (add_zero (binary_list k []) n);;
