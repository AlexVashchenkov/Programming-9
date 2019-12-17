let l = [1;0;1;0;-1;1;0;-1];;

let rec to_3 l = 
	match l with
 [] -> ""
|a :: b -> match a with
	  (-1) -> "$" ^ (to_3 b)
	 |x -> (string_of_int x) ^ (to_3 b);;
