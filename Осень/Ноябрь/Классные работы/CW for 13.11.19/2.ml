open List;;

let rec to_2 n = 
	if n < 0 then failwith"" else
	if n <= 1 then (string_of_int n) else (to_2 (n / 2)) ^ (string_of_int (n mod 2));; 

let rec list_of_string s n = 
	if n > ((String.length s) - 1) then [] else
	(int_of_string (String.make 1 s.[n])) :: (list_of_string s (n+1));;

(*let rec fit l x = 
	if x > (length l) then 0 :: (fit l (x-1)) else
	if x = (length l) then l else failwith"";;*)

let rec sublist l n = 
	match l with
 [] -> []
|a :: b -> if n > 0 then (sublist b (n-1)) else a :: b;;

let rec result l n = ((sublist (rev l) ((length l) - n)),(sublist l n));;

let rec deg_2 n = if n < 0 then failwith"" else (if n = 0 then 1 else 2 * (deg_2 (n-1)));;

let rec string_of_list l = 
	match l with
 [] -> ""
|a :: b -> (string_of_list b) ^ a;;

let rec from_2 s n = 
	if n >= (length s) then 0 else
	(int_of_string (String.make 1 s.[n])) * (deg_2 ((length s) - n - 1)) + (from_2 s (n+1));;

let rec deserialize l n = ((from_2 (sublist (rev l) ((length l) - n)) 0),(sublist l n));;



