let rec make_bool n = 
	let rec bool_list n = 
		if n = 0 then [false] else
		if n = 1 then [true] else
		(if n mod 2 = 0 then false else true) :: (bool_list (n / 2))
	in (List.rev (bool_list n));;

let rec make_first_zeros n k = 
	let n2 = k - (List.length (make_bool n)) in
	if n2 = 0 then (make_bool n) else [false] @ (make_first_zeros n (k-1));;

let init n f = 
	let rec f2 n f = 
		if n = 1 then [(f 0)] else (f n) :: (f2 (n-1) f)
	in List.rev (f2 n f);;

let rec deg_2 n = 
	if n = 0 then 1 else 2 * (deg_2 (n-1));;

let rec list_all_n_places n = 
	List.map (fun x -> make_first_zeros x n) (init (deg_2 n - 1) (fun y -> y));;

let rec zero_to_n n k = 
	let n2 = (deg_2 n) - 1 in
	List.map (fun x -> make_first_zeros x k) (init n2 (fun y -> y));;





  

