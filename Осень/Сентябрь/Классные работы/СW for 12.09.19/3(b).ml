open List;;

let rec create x = 
	if x > 2999 then [] else
	if x mod 100 = 0 then (create (x+1)) else (if x mod 4 = 0 then x :: (create (x+1)) else (create (x+1)));;

let l = 2000 :: (2400 :: (2800 :: (create 2000)));;
 
let probability = 1. /. 900.;;

let rec get_elem l n = 
	match l with
 [] -> failwith""
|a :: b -> if n = 0 then a else (get_elem b n);;

let rec list_to_array l = Array.init 
let rec sum l n =
	if n = (length l) then 0. else
	(float_of_int (get_elem l n)) *. probability +. (sum l (n+1));;

sum l 0;;  
 





                         