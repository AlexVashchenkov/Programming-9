let s = Sys.argv.(1);;
let f = open_in s;;

let rec read_file() = 
	try
		let u = input_byte f in u :: (read_file()) 
	with
		End_of_file -> [];;

let cp1251_to_utf_8 n = 
	if n < 0b10000000 then [n] else
		if n < 0x7ff then [((n lsr 6) lor 0b11000000);((n land 0b00111111) lor 0b10000000)] 
		else [];;

let text = read_file();;

let s2 = Sys.argv.(2);;
let oc = open_out s2;;

List.iter (fun x -> Printf.fprintf oc "%s\n" (List.map (fun x -> string_of_int x) (cp1251_to_utf_8 x))) text;;
	