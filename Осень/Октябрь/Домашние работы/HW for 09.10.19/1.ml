open String;;

let f = open_in "1.txt";;

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
		let u = input_line f in (split_on_char '.' u) :: (read_file())
	with
		End_of_file -> [];;

let list_of_ips = (read_file());;(*List.map (fun x -> (split_on_char '.' x)) ["192.168.0.0";"255.255.255.0";
		   "192.168.0.1";"192.168.0.2";"192.168.0.3";"192.168.0.4";"192.168.0.5"];;*)
close_in f;;

let rec make_8_bit s = 
	if (length s) < 8 then (make (8 - (length s)) '0') ^ s else s;;

let maximum_ip = "255.255.255.255";;
let main_ip = (get_elem list_of_ips 0);;
let mask = (get_elem list_of_ips 1);;
let unenable_ips = List.map (fun x -> concat "." x) (List.tl (List.tl list_of_ips));;

List.iter (fun x -> print_string x;print_string "\n") main_ip;; 
(*кусок программы из прошлой домашки*)
let rec find_all_1 s n = 
	if n = (length s) then 0 else (if s.[n] = '1' then 1 + (find_all_1 s (n+1)) else (find_all_1 s (n+1)));;

let rec string_and s1 s2 n = 
	if n = (length s1) then "" else
	if s1.[n] = s2.[n] && s1.[n] = '1' 
	then "1" ^ (string_and s1 s2 (n+1)) 
	else "0" ^ (string_and s1 s2 (n+1));;

let rec string_xor s1 s2 n = 
	if n = (length s1) then "" else
	if s1.[n] <> s2.[n] 
	then "0" ^ (string_xor s1 s2 (n+1)) 
	else "1" ^ (string_xor s1 s2 (n+1));;

let rec string_not s n = 
	if n = (length s) then "" else
	if s.[n] = '0' 
	then "1" ^ (string_not s (n+1)) 
	else "0" ^ (string_not s (n+1));;

let rec deg_2 n = 
	if n = 0 then 1 else 2 * (deg_2 (n-1));;

let rec to_binary n = 
	if n < 0 then failwith"" else
	if n = 0 then "0" else
	if n = 1 then "1" else
	(to_binary (n / 2)) ^ (string_of_int (n mod 2));;

let rec from_binary s n = 
	if n = (length s) then 0 
	else ((int_of_string (make 1 s.[n])) * (deg_2 ((length s)-n-1))) + (from_binary s (n+1));;


let rec split_by_8 s n = 
	if n >= (length s) - 1 then [] else (sub s n 8) :: (split_by_8 s (n+8));;

let plus s = 
	let rec binary_plus s index = 
		if index = -1 then "1" else
		if s.[index] = '1' then (binary_plus (sub s 0 index) (index - 1)) ^ "0" else (sub s 0 (index)) ^ "1"
	in (binary_plus s ((length s) - 1));;

let l1 = List.map (fun x -> (make_8_bit (to_binary (int_of_string x)))) main_ip;;
let l2 = List.map (fun x -> (make_8_bit (to_binary (int_of_string x)))) mask;;
let length_mask = find_all_1 (concat "" l2) 0;; 
let pattern = List.map2 (fun x -> (fun y -> (string_and x y 0))) l1 l2;;
 
let max_ip = List.map2 (fun x -> (fun y -> (string_xor x y 0))) l1 l2;;

let rec list_of_ip ip = 
	if ip >= max_ip 
	then [] 
	else ip :: (list_of_ip (split_by_8 (plus (concat "" ip)) 0));;

let lst = List.map (fun x -> (concat "." (List.map (fun y -> (string_of_int (from_binary y 0))) x))) (List.tl (list_of_ip pattern));;

let rec check_in l x = 
	match l with
 [] -> false
|a :: b -> if a = x then true else (check_in b x);;

let rec remove l x = 
	match l with
 [] -> []
|a :: b -> if a = x then b else a :: (remove b x);;

let rec delete lst unenable_ips = 
	match unenable_ips with
 [] -> lst 
|a :: b -> if (check_in lst a) = true then (delete (remove lst a) b) else (delete lst b);;

let rec delete_255 lst = 
	match lst with
 [] -> lst
|a :: b -> if (get_elem (split_on_char '.' a) 3) = "255" then (delete_255 b) else a :: (delete_255 b);;

let y = open_out "1.txt";;

let main() = 
Printf.fprintf y "%s\n" (concat "." main_ip);
Printf.fprintf y "%s\n" (concat "." mask);
if (List.length (delete lst unenable_ips)) = 0 
then Printf.fprintf y "no available ips"
else Printf.fprintf y "%s" (List.hd (delete_255 (delete lst unenable_ips)));;

List.iter (fun x -> print_string (x ^ " ")) main_ip;;
print_string "\n";;
List.iter (fun x -> print_string (x ^ " ")) mask;;
print_string "\n";;
List.iter (fun x -> print_string (x ^ "\n")) unenable_ips;;
print_string "\n";;
(*List.iter (fun x -> print_string x;print_string " ") lst;;*)

main();;
close_out (open_out "1.txt");;                                                              4