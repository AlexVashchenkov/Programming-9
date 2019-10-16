open String;;

let ip = "117.94.100.0";;
let mask = "255.255.240.0";;
let address = "117.94.100.1";;

let rec find_all_1 s n = 
	if n = (length s) then 0 else (if s.[n] = '1' then 1 + (find_all_1 s (n+1)) else (find_all_1 s (n+1)));;

let rec to_binary n = 
	if n < 0 then failwith"" else
	if n = 0 then "0" else
	if n = 1 then "1" else
	(to_binary (n / 2)) ^ (string_of_int (n mod 2));;

let rec get_elem l n = 
	match l with
 [] -> failwith""
|a :: b -> if n = 0 then a else (get_elem b (n-1));;
 
let split_on_char sep s = 
	let rec split_on sep s n x = 
		if n = (length s) then [x] else 	
		if s.[n] = sep then x :: (split_on sep s (n+1) "") else (split_on sep s (n+1) (x ^ (make 1 s.[n])))
	in (split_on sep s 0 "");;

let rec string_and s1 s2 n = 
	if n = (length s1) then "" else
	if s1.[n] = s2.[n] && s1.[n] = '1' 
	then "1" ^ (string_and s1 s2 (n+1)) 
	else "0" ^ (string_and s1 s2 (n+1));;

let rec deg_2 n = 
	if n = 0 then 1 else 2 * (deg_2 (n-1));;

let rec from_binary s n = 
	if n = (length s) then 0 
	else ((int_of_string (make 1 s.[n])) * (deg_2 ((length s)-n-1))) + (from_binary s (n+1));;

let rec make_8_bit s = 
	if (length s) < 8 then (make (8 - (length s)) '0') ^ s else s;;

let l1 = List.map (fun x -> (make_8_bit (to_binary (int_of_string x)))) (split_on_char '.' ip);;
let l2 = List.map (fun x -> (make_8_bit (to_binary (int_of_string x)))) (split_on_char '.' mask);;
let length_mask = find_all_1 (concat "" l2) 0;; 
let pattern = List.map2 (fun x -> (fun y -> (string_and x y 0))) l2 l1;;

let new_address = (concat "" (List.map (fun x -> (make_8_bit (to_binary (int_of_string x)))) (split_on_char '.' address)));;

Printf.printf "%B" ((sub new_address 0 length_mask) = (sub (concat "" pattern) 0 length_mask));;