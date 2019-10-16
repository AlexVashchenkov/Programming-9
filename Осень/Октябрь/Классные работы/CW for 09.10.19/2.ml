let s = Sys.argv.(1);;
let f = open_in_bin s;;

let rec read_file() = 
	try
		let u = input_byte f in u :: (read_file())
	with
		End_of_file -> [];;

let text = read_file();;

let s2 = Sys.argv.(2);;
let oc = open_out_bin s2;;

List.iter (fun x -> output_byte oc x) text;;
	