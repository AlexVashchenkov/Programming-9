open String;;

let l = ["00";"01";"10";"11"];;

let rec list_of_string s n = 
	if n >= (length s) then [] else [(int_of_string (String.make 1 s.[n]))] @ (list_of_string s (n+1));;

let lst = List.map (fun x -> (list_of_string x 0)) l;;

let rec mixes (a,b) = 
		match (a,b) with 

	 (a1::al,b1::bl) -> if a1 = b1 then (mixes (al,bl)) else true
	|(_,_) -> false;;
 
let rec delete l x = 	
	match l with
 [] -> []
|a :: b -> if a = x then b else a :: (delete b x);;

let rec mix2 l s = 
	match l with
 [] -> []
|a :: b -> (mixes (a,s)) :: (mix2 l s);;
 
let rec is_prefix l l2 = 
	match l with
 [] -> []
|a :: b -> (mix2 (delete l2 a) a) :: (is_prefix b l2);;


  