open String;;

let s = Sys.argv.(1);;
let f = open_in s;;

let rec read_file() = 
	try
		let u = input_line f in u :: (read_file())
	with
		End_of_file -> [];;

let rec find_max l x = 
	match l with
 [] -> x
|a :: b -> if (length a) >= (length x) then (find_max b a) else (find_max b x);;

let rec delete_max l = 
	match l with
 [] -> []
|a :: b -> if a = (find_max l (List.hd l)) then b else a :: (delete_max b);;

let text = read_file();;

let s2 = Sys.argv.(2);;
let oc = open_out s2;;

List.iter (fun x -> Printf.fprintf oc "%s\n" x) (delete_max text);;
