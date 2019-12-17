open Array;;

let l =   [|0;1;2;3;4;5;6;7|];;

let a = [|[|0;1;1;1;1;0;0;0|]
	 ;[|0;0;0;0;0;1;0;0|]
	 ;[|0;0;0;0;0;1;1;0|]
	 ;[|0;0;0;0;0;0;1;0|]
	 ;[|0;0;0;0;0;0;1;0|]
	 ;[|0;0;0;0;0;0;0;1|]                             
	 ;[|0;0;0;0;0;0;0;1|]
	 ;[|0;0;0;0;0;0;0;0|]|];;

let rec forall a n = 
	let rec check_zero a m n = 
		if m = (length a) then true else
		if a.(m).(n) = 0 then (check_zero a (m+1) n) else false
	in (if n = (length a.(0)) then [] else (if (check_zero a 0 n) = true then l.(n) :: (forall a (n+1)) else (forall a (n+1))));; 

 