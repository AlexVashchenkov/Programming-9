let lst = [254;182;63];;

let rec to_2 n = 
	if n < 0 then failwith"" else
	if n <= 1 then [n] else (to_2 (n / 2)) @ [(n mod 2)];;

let join_bytes = flatten (List.map (fun x -> (to_2 x)) lst);; 
