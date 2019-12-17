open Array;;

let a = [|[|0;1;1;1;1;0;0;0|]
	 ;[|0;0;0;0;0;1;0;0|]
	 ;[|0;0;0;0;0;1;1;0|]
	 ;[|0;0;0;0;0;0;1;0|]
	 ;[|0;0;0;0;0;0;1;0|]
	 ;[|0;0;0;0;0;0;0;1|]                             
	 ;[|0;0;0;0;0;0;0;1|]
	 ;[|0;0;0;0;0;0;0;0|]|];;

let rec find_min a m n = 
	if n >= (length a) then true else
	if a.(n).(m) = 0 then (find_min a m (n+1)) else false;;

let rec check_digit a n = 
	if n >= (length a) then [] else (if (find_min a n 0) = true then a.(n) :: (check_digit    