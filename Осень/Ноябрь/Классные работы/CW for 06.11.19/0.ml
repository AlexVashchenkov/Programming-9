open String;;

let s = "dimoooooooooooooonindddddii";;

let rec count s n x = 
	if n = (length s) then 0 else 
	if s.[n] = x then 1 + (count s (n+1) x) else (count s (n+1) x);;

let rec find l x = 
	match l with
 [] -> 0
|a :: b -> if a = x then 1 + (find b x) else (find b x);;

let rec make_uniq s n l = 
	if n = (length s) then l else
	if (find l s.[n]) = 0 then (make_uniq s (n+1) (s.[n] :: l)) else (make_uniq s (n+1) l);;
 
let rec a = List.map (fun x -> (x,((float_of_int (count s 0 x)) /. (float_of_int (length s))))) (make_uniq s 0 []);;

