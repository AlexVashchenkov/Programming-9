open Array;;

let l = [|1;4;2;7;5;9;18|];;

let rec num l x n = 
	if n >= (length l) then failwith"num" else
	if l.(n) = x then n else (num l x (n+1));;
                                                                            
let rec find_all_bigger l x n = 
	if n >= (length l) then [] else
	if x < l.(n) then l.(n) :: (find_all_bigger l l.(n) (n+1)) else (find_all_bigger l x (n+1));;

let rec max a b = 
	if (List.length a) > (List.length b) then a else
	if (List.length a) < (List.length b) then b else
	match (a,b) with
 ([],[]) -> []
|(x :: y, w :: z) -> if x < w then b else (if x > w then a else (max y z));;

let rec maximum l x = 
	match l with
 [] -> x 
|a :: b -> if (max a x) = a then (maximum b a) else (maximum b x);;

let rec make_list n = 
	if n >= (length l) then [] else (find_all_bigger l l.(n) n) :: (make_list (n+1));; 

let rec get_elem l n = 
	match l with
 [] -> failwith"get_elem"
|a :: b -> if n = 0 then a else (get_elem b (n-1));;

(maximum (make_list 0) (get_elem (make_list 0) 0));;
