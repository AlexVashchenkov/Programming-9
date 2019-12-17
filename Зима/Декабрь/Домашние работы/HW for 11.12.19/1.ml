open Array;;

let a = [|[|0;1;1;1;1;0;0;0|]
	 ;[|0;0;0;0;0;1;0;0|]
	 ;[|0;0;0;0;0;1;1;0|]
	 ;[|0;0;0;0;0;0;1;0|]
	 ;[|0;0;0;0;0;0;1;0|]
	 ;[|0;0;0;0;0;0;0;1|]                             
	 ;[|0;0;0;0;0;0;0;1|]
	 ;[|0;0;0;0;0;0;0;0|]|];;

let rec count_vertices a m n = 
	if m = (length a) then 0 else
	if n = (length a.(m)) then (count_vertices a (m+1) 0) else 
	if a.(m).(n) = 1 then 1 + (count_vertices a m (n+1)) else (count_vertices a m (n+1));;

count_vertices a 0 0;; 