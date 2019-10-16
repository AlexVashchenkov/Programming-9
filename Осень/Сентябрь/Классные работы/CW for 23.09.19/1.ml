let f = open_in "1.txt";;

let rec v_read_file p = 

	try
		let u = input_line f in (v_read_file (p+1))
	with 
		End_of_file -> p;;

print_int (v_read_file 0);;

close_in f;;
