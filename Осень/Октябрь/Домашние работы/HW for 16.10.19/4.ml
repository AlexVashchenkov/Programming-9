let f = open_in "4.txt";;

let rec read_file() = 
	try
		let u = input_byte f in u :: read_file()
	with
		End_of_file -> [];;

let l = read_file();;

let cp1251_to_utf_8 n = 
	if n < 0b10000000 then [n] else
		if n < 0x7ff then [((n lsr 6) lor 0b11000000);((n land 0b00111111) lor 0b10000000)] 
		else [];;

let s2 = "4.txt";;
let oc = open_out s2;;

List.iter (fun x -> List.iter (fun y -> Printf.fprintf oc "%d " y) x) (List.map (fun x -> (cp1251_to_utf_8 x)) l);;
close_out oc;;
let f2 = open_in "4.txt";;

let rec read_file2() = 
	try
		let u = input_byte f2 in u :: read_file2()
	with
		End_of_file -> [];;

let l2 = read_file2();;

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

let arr = List.map (fun x -> (count l x)) (make_uniq l2);;

List.iter (fun x -> print_int x;print_string "\n") arr;;

let rec sum l = 
	match l with 
 [] -> 0
|a :: b -> a + (sum b);;

let rec varieties = List.map (fun x -> (float_of_int x) /. (float_of_int (sum arr))) arr;;

let rec shennon l = 
	match l with
 [] -> 0.
|a :: b -> if a = 0. then (shennon b) else (-. (a *. (log a) /. (log 2.))) +. (shennon b);;

print_string "\n";;
print_float (shennon varieties);;
