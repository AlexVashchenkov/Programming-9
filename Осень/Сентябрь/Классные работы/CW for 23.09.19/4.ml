open String;;

let f = open_in "1.txt";;

let rec count_a s n = 
	if n = (length s) then 0 else
	if s.[n] = 'à' then 1 + (count_a s (n+1)) else (count_a s (n+1));;

let rec v_read_file p = 

	try
		let u = input_line f in ((print_int (count_a u 0));(print_string "\n");(v_read_file()))
	with 
		End_of_file -> ();;

(v_read_file());;

close_in f;;
