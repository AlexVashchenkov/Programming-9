open List;;

let l = [0;1;2;3;4;5;6;7];;

let a =  [[0;1;1;1;0;0;0;0];
	  [0;0;0;0;1;0;0;0];
	  [0;0;0;0;1;1;0;0];
	  [0;0;0;0;0;1;1;0];
	  [0;0;0;0;0;0;0;1];
	  [0;0;0;0;0;0;0;1];
	  [0;0;0;0;0;1;0;0];
	  [0;0;0;0;0;0;0;0]];;

let rec get_elem l n = 
	match l with
 [] -> failwith"get_elem"
|a :: b -> if n = 0 then a else (get_elem b (n-1));;

let rec check_zero a index x = 
	if index = (length a) then true else
	if (get_elem (get_elem a index) x) = 0 then (check_zero a (index+1) x) else false;;

let rec find_all_zero a n =
	if n = (length a) then [] else
	if (check_zero a 0 n) = true then n :: (find_all_zero a (n+1)) else (find_all_zero a (n+1));;

let rec delete_elem a n = 
	match a with
 [] -> []
|a :: b -> if n = 0 then b else a :: (delete_elem b (n-1));;

let rec delete_column a n = List.map (fun x -> (delete_elem x n)) a;;

let rec delete_from_list q l = 
	match l with
 [] -> q
|a :: b -> (delete_from_list (delete_elem (delete_column q a) a) b);;

let rec delete_elems q l = 
	match l with
 [] -> q
|a :: b -> (delete_elems (delete_elem q a) b);;

let rec top_sort a l = 
	if a = [] 
	then [] 
	else (List.map (fun x -> (get_elem l x)) (find_all_zero a 0)) :: (top_sort (delete_from_list a (find_all_zero a 0)) (delete_elems l (find_all_zero a 0)));;
	
(top_sort a l);;
