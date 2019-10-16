let f = open_in_bin "x.txt";;

let rec v_read_file() = 

	try
		let u = input_byte f in  
	with 
		End_of_file -> false;;

