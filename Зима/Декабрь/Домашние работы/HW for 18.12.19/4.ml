open String;;

let s1 = "$0111$";;
let s2 = "01$011";;

let rec from_3 s n = 
	if n = (length s) then [] else
	if s.[n] = '$' then (-1) :: (from_3 s (n+1)) else (int_of_string (make 1 s.[n])) :: (from_3 s (n+1));;

let rec get_elem l n = 	
	match l with
 [] -> failwith""
|a :: b -> if n = 0 then a else (get_elem b (n-1));;

let rec to_3 l = 
	match l with
 [] -> ""
|a :: b -> match a with
	  (-1) -> "$" ^ (to_3 b)
	 |x -> (string_of_int x) ^ (to_3 b);;

let rec sum2 l1 l2 n p = 
	if n = (List.length l1) then (if p = 0 then [] else [p]) else
	let k = (get_elem l1 ((List.length l1) - n - 1)) + (get_elem l2 ((List.length l2) - n - 1)) in
	match (k+p) with
 (-3) -> [0] @ (sum2 l1 l2 (n+1) (-1))
|(-2) ->    [1] @ (sum2 l1 l2 (n+1) (-1))
|(-1) -> [(-1)] @ (sum2 l1 l2 (n+1) 0)
|0 -> [0] @ (sum2 l1 l2 (n+1) 0)
|1 -> [1] @ (sum2 l1 l2 (n+1) 0)
|2 -> [1] @ (sum2 l1 l2 (n+1) (-1))
|3 -> [0] @ (sum2 l1 l2 (n+1) 1);;

(List.rev (sum2 (from_3 s1 0) (from_3 s2 0) 0 0));;