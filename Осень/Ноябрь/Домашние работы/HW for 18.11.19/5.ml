open List;;

let f = open_in "5.txt";;

let rec read_file() = 
	try
		let s = input_line f in (read_file()) ^ s
	with
		End_of_file -> "";; 

let str = read_file();;

type tree = Node of tree * tree | Leaf of string;;

let rec find l x = 
	match l with
 [] -> false
|a :: b -> if a = x then true else (find b x);;

let rec dict s n l = 
	if n = (String.length s) then l else
if (find l (String.make 1 s.[n])) = false 
then (dict s (n+1) ([(String.make 1 s.[n])] @ l)) 
else (dict s (n+1) l);;

let rec count s n x = 
	if n = (String.length s) then 0 else 
	if (String.make 1 s.[n]) = x then 1 + (count s (n+1) x) else (count s (n+1) x);;

let lst = List.map (fun x -> (Leaf x,(count str 0 x))) (dict str 0 []);;

let rec get_elem l n = 
	match l with
 [] -> failwith""
|a :: b -> if n = 0 then a else (get_elem b (n-1));;

let rec delete lst x = 
	match lst with
 [] -> []
|a :: b -> if a = x then b else a :: (delete b x);;

let min lst = 
	let rec minimum l x = 
		match l with 
 	 [] -> x
	|a :: b -> if (snd a) < (snd x) then (minimum b a) else (minimum b x)
in (minimum lst (hd lst));;

let rec plus p1 p2 = (Node ((fst p1),(fst p2)),(snd p1) + (snd p2));;

let rec hafmann lst = 
	let rec step lst = 
		let min1 = (min lst) in
		let min2 = (min (delete lst min1)) in
		let min_pair = plus min1 min2 in
		(delete (delete lst min1) min2) @ [min_pair]
	in (if (length lst) = 1 then (fst (get_elem lst 0)) else (hafmann (step lst)));;

List.iter (fun (Leaf x,y) -> print_string "(Leaf ";print_string x;print_string ",";print_int y;print_string ")") (rev lst);;
print_string "\n";;

let rec print_tree tree = 
	match tree with
 Leaf x -> print_string "(Leaf "; print_string x ; print_string ")"
|Node (t1,t2) -> print_string "Node ";print_tree t1;print_string ",";print_tree t2;print_string "";;

print_tree (hafmann (rev lst));;