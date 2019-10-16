open String;;

let f = open_in_bin "5.txt";;

let rec find u x n1 n2 = 
	if n2 >= (length u) && n1 >= ((length u) - (length x) + 1) then false else
	if (sub u n1 n2) = x then true else (find u x (n1+1) n2);;
	
let rec v_read_file() = 

	try
		let u = input_line f in (if (find u ("111001111000111010001011") 0 (length "111001111000111010001011")) then true else v_read_file())
	with 
		End_of_file -> false;;

Printf.printf "%B" (v_read_file());;

close_in f;;
