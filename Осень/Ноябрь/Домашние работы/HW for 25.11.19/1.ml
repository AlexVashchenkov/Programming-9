open List;;

let rec to_2 n = 
	if n < 0 then failwith"" else
	if n <= 1 then (string_of_int n) else (to_2 (n / 2)) ^ (string_of_int (n mod 2));; 

let rec list_of_string s n = 
	if n > ((String.length s) - 1) then [] else
	(int_of_string (String.make 1 s.[n])) :: (list_of_string s (n+1));;

let rec fit l x = 
	if x > (length l) then 0 :: (fit l (x-1)) else
	if x = (length l) then l else failwith"";;

let rec serialize lst x n = 
	(fit (list_of_string (to_2 n) 0) x) @ lst;;


