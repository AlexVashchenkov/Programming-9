open List;;

let l = [0;1;2;3;4;5];;

let a =  [[0;1;1;0;0;0];
	  [0;0;0;1;0;0];
	  [0;1;0;1;1;0];
	  [0;0;0;0;0;1];
	  [0;0;0;0;0;1];
	  [0;0;0;0;0;0]];;

let rec make_spec n k = 
	if n = 0 then [] else k :: (make_spec (n-1) k);;

let rec get_elem l n = 
	match l with
 [] -> failwith"get_elem"
|a :: b -> if n = 0 then a else (get_elem b (n-1));;

let rec check_first l x n = 
	if n = (length l) then true else 
	if (get_elem (get_elem l n) x) <> 1 then (check_first l x (n+1)) else false;;

let rec find_all_first l n = 
	if n = (length l) then [] else
	if ((check_first l n 0) && 
	(List.map (fun x -> (get_elem (get_elem l x) n)) (make_spec (length l) 0) <> (make_spec (length l) 2))) then n :: (find_all_first l (n+1)) else (find_all_first l (n+1));;

let rec replace_row q n = 
	match q with
 [] -> []
|a :: b -> if n = 0 then (make_spec (length l) 2) :: b else a :: (replace_row b (n-1));;

let rec replace_rows l1 l2 = 
	match l2 with
 [] -> l1
|a :: b -> (replace_rows (replace_row l1 a) b);;
  
let rec replace l n = 
	match l with
 [] -> []
|a :: b -> if n = 0 then 2 :: b else a :: (replace b (n-1));;
	 
let rec replace_column l n = List.map (fun x -> (replace x n)) l;;

let rec replace_columns l1 l2 = 
	match l2 with
 [] -> l1
|a :: b -> (replace_columns (replace_column l1 a) b);;

let rec main a = 
	if a = List.map (fun x -> (make_spec (length a) 2)) (make_spec (length a) 2) then [] else 
	let l = (find_all_first a 0) in
	if l = [] then [] else
	l @ (main (replace_rows (replace_columns a l) l));;

let rec print_matrix l = 
	match l with
 [] -> ()
|a :: b -> List.iter (fun x -> print_int x;print_string " ") a;print_string "\n";print_matrix b;;
print_matrix a;;
List.map (fun x -> (get_elem (get_elem a x) 0)) (make_spec (length a) 0);;
