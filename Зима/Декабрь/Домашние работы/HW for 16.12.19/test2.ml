open List;;

let l = [0;1;2;3;4;5];;

let a =  [[0;1;1;0;0;0];
	  [0;0;0;1;0;0];
	  [0;1;0;1;1;0];
	  [0;0;0;0;0;1];
	  [0;0;0;0;0;1];
	  [0;0;0;0;0;0]];;

let rec delete_elem l x = 
	match l with
 [] -> []
|a :: b -> if a = x then b else a :: (delete_elem b x);;

let rec delete_index l n = 
	match l with
 [] -> []
|a :: b -> if n = 0 then b else a :: (delete_elem b (n-1));;
	
let rec delete_elems l1 l2 = 
	match l2 with
 [] -> l1
|a :: b -> (delete_elems (delete_index l1 a) b);;
		 
let rec get_elem l n = 
	match l with
 [] -> failwith"get_elem"
|a :: b -> if n = 0 then a else (get_elem b (n-1));;

let rec check_first l x n = 
	if n = (length l) then true else 
	if (get_elem (get_elem l n) x) <> 1 then (check_first l x (n+1)) else false;;

let rec find_all_first l n = 
	if n = (length l) then [] else
	if (check_first l n 0) then n :: (find_all_first l (n+1)) else (find_all_first l (n+1));;

let rec delete_column l n = List.map (fun x -> (delete_index x n)) l;;

let rec delete_columns l1 l2 = 
	match l2 with
 [] -> l1
|a :: b -> (delete_columns (delete_column l1 a) b);;

let rec main a = 
	if a = [] then [] else 
	let l_ = (find_all_first a 0) in
	l @ (main (delete_elems (delete_columns a l) l));;
