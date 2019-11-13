open String;;

let lst = [1;1;1;1;1;1;1;0;1;0;1;1;0;0;1;0];;

let rec split_by_8 lst = 
	match lst with
 [] -> []
|a1::(a2 :: (a3 :: (a4 :: (a5 :: (a6 :: (a7 :: (a8 :: b))))))) -> [[a1;a2;a3;a4;a5;a6;a7;a8]] @ (split_by_8 b);;

let rec deg_2 n = if n < 0 then failwith"" else (if n = 0 then 1 else 2 * (deg_2 (n-1)));;

let rec string_of_list l = 
	match l with
 [] -> ""
|a :: b -> (string_of_list b) ^ a;;

let rec from_2 s n = 
	if n >= (length s) then 0 else
	(int_of_string (String.make 1 s.[n])) * (deg_2 ((length s) - n - 1)) + (from_2 s (n+1));;

let split_to_bytes lst = List.map (fun x -> (from_2 (string_of_list x))) lst;;

split_to_bytes (split_by_8 lst);;
