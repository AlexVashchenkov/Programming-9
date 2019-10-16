let f = open_in "in3.txt";;

open String;;

let rec zip_line s n x = 
	if n = (length s) then [x] else 
	if s.[n] =  ' ' then x :: (zip_line s (n+1) "") else (zip_line s (n+1) (x ^ (make 1 s.[n])));;

let rec all_words() = 	
	try
		let u = input_line f in (zip_line u 0 "") @ (all_words())
	with
		End_of_file -> [];;

let rec number = (List.length (all_words()));;

let rec 