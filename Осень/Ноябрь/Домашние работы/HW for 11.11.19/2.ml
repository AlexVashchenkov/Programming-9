open List;;

type tree = Node of tree * tree | Leaf of string;;

let lst = [(Leaf "x",10);(Leaf "y",12);(Leaf "x",4)];;

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
 
