open List;;

let n = 134;;

let rec to_3 n = 
	if n <= 2 then [n] else (n mod 3) :: (to_3 (n / 3));;

let rec main l p = 
	match l with
 [] -> [p]
|a :: b -> match a+p with
	  (-3) -> 0 :: (main b (-1)) 
	  |(-2) -> 1 :: (main b (-1)) 
	  |(-1) -> (-1) :: (main b 0) 
	    | 0 -> 0 :: (main b 0) 
	    | 1 -> 1 :: (main b 0) 
	    | 2 -> (-1)  :: (main b 1) 
	    | 3 -> 0 :: (main b 1);;

 
	