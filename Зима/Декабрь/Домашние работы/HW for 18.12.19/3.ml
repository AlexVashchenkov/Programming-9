open String;;

let s = "1010$10$";;

let rec from_3 s n = 
	if n = (length s) then [] else
	if s.[n] = '$' then (-1) :: (from_3 s (n+1)) else (int_of_string (make 1 s.[n])) :: (from_3 s (n+1));;