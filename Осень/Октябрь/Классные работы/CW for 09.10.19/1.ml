let s = Sys.argv.(1);;
let f = open_in s;;

let rec read_file() = 
	try
		let u = input_line f in u :: (read_file())
	with
		End_of_file -> [];;

let text = read_file();;

let s2 = Sys.argv.(2);;
let oc = open_out s2;;

List.iter (fun x -> Printf.fprintf oc "%s\n" x) text;;
	