open String;;

let address = "192.169.0.1";;


let split_on_char sep s = 
	let rec split_on sep s n x = 
		if n = (length s) then [x] else 	
		if s.[n] = sep then x :: (split_on sep s (n+1) "") else (split_on sep s (n+1) (x ^ (make 1 s.[n])))
	in (split_on sep s 0 "");;

let rec get_elem l n = 
	match l with
 [] -> failwith""
|a :: b -> if n = 0 then a else (get_elem b (n-1));;

let check_first address = 
		if (get_elem (split_on_char '.' address) 0) = "10" then true else false;;
let check_second address = 
	if (int_of_string (get_elem (split_on_char '.' address) 0)) = 172 && 
	   (int_of_string (get_elem (split_on_char '.' address) 1)) >= 16 &&
	   (int_of_string (get_elem (split_on_char '.' address) 0)) <= 31 then true else false;;

let check_third address = 
	if (get_elem (split_on_char '.' address) 0) = "192" && 
	   (get_elem (split_on_char '.' address) 1) = "168" then true else false;;

let main address = 
	if (check_first address) || (check_second address) || (check_third address) then print_string "True" else print_string "False";;
	
main address;;
	
