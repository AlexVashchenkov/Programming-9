open List;;
open String;;

let rec find l x = 
	match l with
 [] -> false
|a :: b -> if a = x then true else (find b x);;

let rec plus l n = 
	match l with
 [] -> []
|(a,b) :: x -> if b = n then ((a + 1,b) :: x) else (a,b) :: (plus x n);;

let rec different h s n = 
	if n = (length s) then h else
	if (find h s.[n]) = false then (0,s.[n]) :: (different h s (n+1)) else
	(different l s (n+1));;

let rec all l h s n = 
	if n = (length s) then l else
	if (find l s.[n]) = false then (0,s.[n]) :: (all l s (n+1)) else
	(plus (all l s (n+1)) s.[n]);;