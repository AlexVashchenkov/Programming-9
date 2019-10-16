open String;;

let n = read_int();;

let rec main n l = 
	if (List.length l) = 1 && n < 255 then [0;0;n] @ l else
        if (List.length l) = 2 && n < 255 then [0;n] @ l else
	if (List.length l) = 3 && n < 255 then [n] @ l else
	if n > 255 then (main (n / 256) ((n mod 256) :: l)) else (n :: l);;

let rec print_list l = 
	match l with 
 [] -> ()
|a :: b -> if b = [] then (print_int a) else (print_int a;print_string ".";(print_list b));;

(*List.iter (fun x -> print_int x) (List.rev (main n));;*)
print_list (main n []);; 


