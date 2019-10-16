let f = open_in "3.txt";;

let rec read_file() = 
	try
		let u = input_byte f in u :: read_file()
	with
		End_of_file -> [];;

let l = read_file();;

let rec find l x = 
	match l with
 [] -> false
|a :: b -> if a = x then true else (find b x);;

let rec count l x = 
	match l with
 [] -> 0
|a :: b -> if a = x then 1 + (count b x) else (count b x);;

let rec make_uniq l = 
	match l with
 [] -> []
|a :: b -> if (count l a) > 1 then (make_uniq b) else a :: (make_uniq b);;

let arr = List.map (fun x -> (count l x)) (make_uniq l);;

List.iter (fun x -> print_int x;print_string "\n") arr;;

let rec sum l = 
	match l with 
 [] -> 0
|a :: b -> a + (sum b);;

let rec varieties = List.map (fun x -> (float_of_int x) /. (float_of_int (sum arr))) arr;;

let rec shennon l = 
	match l with
 [] -> 0.
|a :: b -> (-. (a *. (log a) /. (log 2.))) +. (shennon b);;

print_string "\n";;
print_float (shennon varieties);;
