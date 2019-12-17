open String;;

let l = ["00";"01";"10";"11"];;

let rec list_of_string s n = 
	if n >= (length s) then [] else [(int_of_string (make 1 s.[n]))] @ (list_of_string s (n+1));;

let rec get_elem l n = 
	match l with
 [] -> failwith""
|a :: b -> if n = 0 then a else (get_elem b (n-1));;

let rec check_useless l n = 
	