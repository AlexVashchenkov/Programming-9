open String;;

let s = "22.173.235.102";;

let split_on_char sep s = 
	let rec split_on sep s n x = 
		if n = (length s) then [x] else 	
		if s.[n] = sep then x :: (split_on sep s (n+1) "") else (split_on sep s (n+1) (x ^ (make 1 s.[n])))
	in (split_on sep s 0 "");;

let rec get_elem l n = 
	match l with
 [] -> failwith""
|a :: b -> if n = 0 then a else (get_elem b (n-1));;

let l = List.map (fun x -> (int_of_string x)) (split_on_char '.' s);;

let rec result = (get_elem l 0) * (int_of_float (256. ** 3.)) + 
		 (get_elem l 1) * (int_of_float (256. ** 2.)) +
		 (get_elem l 2) * (int_of_float (256.)) + (get_elem l 3);;

List.iter (fun x -> print_string x;print_string " ") (split_on_char '.' s);;
print_string "\n";;
print_int result;;

