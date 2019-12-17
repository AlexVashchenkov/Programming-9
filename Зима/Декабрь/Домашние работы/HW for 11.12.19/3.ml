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

let rec sum l = 
	match l with 
 [] -> 0
|a :: b -> a + (sum b);;

let rec count_out a x n = 
	if x = (length a) then 0 else a.(x).(n) + (count_out a (x+1) n);;

let rec count_in a x n = 
	if x = (length a) then 0 else a.(n).(x) + (count_out a (x+1) n);;

let rec top_sort q l = 
	match l with
 [] -> []
|a :: b -> if (count_in q 0 a) = 0 then a :: (top_sort q b) else (top_sort   
