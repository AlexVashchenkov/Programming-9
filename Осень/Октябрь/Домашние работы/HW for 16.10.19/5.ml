let f = open_in "5.txt";;

let rec read_file() = 
	try
		let u = input_byte f in u :: (read_file())
	with
		End_of_file -> [];;

let l = read_file();;

let weight x = if x < 128 then 1 else 2;;

let rec lst = List.map (fun x -> (weight x)) l;;

let rec sum l = 
	match l with 
 [] -> 0
|a :: b -> a + (sum b);;

print_float ((float_of_int (sum lst)) /. (float_of_int (List.length lst)));;

