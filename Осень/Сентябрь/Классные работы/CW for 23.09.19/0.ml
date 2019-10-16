let f = open_in "1.txt";;

let rec v_read_file()= 

	try
		let u = input_line f in u ^ (v_read_file())
	with 
		End_of_file -> "";;

print_string (v_read_file());;

close_in f;;
