open String;;

let f = open_in "2.txt";;

let split_on_char sep s = 
	let rec split_on sep s n x = 
		if n = (length s) then [x] else 	
		if s.[n] = sep then x :: (split_on sep s (n+1) "") else (split_on sep s (n+1) (x ^ (make 1 s.[n])))
	in (split_on sep s 0 "");;

let rec get_elem l n = 
	match l with
 [] -> failwith""
|a :: b -> if n = 0 then a else (get_elem b (n-1));;

let rec read_file() = 
	try
		let u = input_line f in (split_on_char '.' u) :: read_file()
	with
		End_of_file -> [];;

let rec string_and s1 s2 n = 
	if n = (length s1) then "" else
	if s1.[n] = s2.[n] && s1.[n] = '1' 
	then "1" ^ (string_and s1 s2 (n+1)) 
	else "0" ^ (string_and s1 s2 (n+1));;

let rec to_binary n = 
	if n < 0 then failwith"" else
	if n = 0 then "0" else
	if n = 1 then "1" else
	(to_binary (n / 2)) ^ (string_of_int (n mod 2));;

let rec make_8_bit s = 
	if (length s) < 8 then (make (8 - (length s)) '0') ^ s else s;;

let list_of_ips = read_file();;

let main_ip = (get_elem list_of_ips 0);;

let mask = (get_elem list_of_ips 1);;

let unenable_ips = List.tl (List.tl list_of_ips);;

let l1 = List.map (fun x -> (make_8_bit (to_binary (int_of_string x)))) main_ip;;
let l2 = List.map (fun x -> (make_8_bit (to_binary (int_of_string x)))) mask;;

let pattern = List.map2 (fun x -> (fun y -> (string_and x y 0))) l1 l2;;

let rec find_all l x = 
	match l with
 [] -> 0
|a :: b -> if a = x then 1 + (find_all b x) else (find_all b x);;
                                                                                                                           
let rec check l = 
	match l with
 [] -> true
|a :: b -> if (find_all l a) = 1 then (check b) else false;;    

let rec check_2 l = 
	match l with
 [] -> true
|a :: b -> if (List.map2 (fun x -> (fun y -> string_and x y 0)) a (List.map (fun x -> (make_8_bit x)) mask)) = pattern then (check b) else false;;    

List.iter (fun x -> print_string x) pattern;;
print_string "\n";;
List.iter (fun x -> Printf.printf "%s\n" (concat "." x)) unenable_ips;;
Printf.printf "%B" ((check unenable_ips) && (check_2 (List.map (fun x -> List.map (fun y -> to_binary (int_of_string y)) x) unenable_ips)));; 	