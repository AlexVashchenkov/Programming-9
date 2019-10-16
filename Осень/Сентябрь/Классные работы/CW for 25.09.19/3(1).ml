let f = open_in "x.txt";;

let rec space p n = 	
	if n = (String.length p) then "" else 
	if p.[n] = ' ' then "\n" ^ (space p (n+1)) else (make 1 p.[n]) ^ (space p (n+1));;


let rec v_read_file l = 

	try
		let u = input_line f = 
	with                    
		End_of_file -> l;;
