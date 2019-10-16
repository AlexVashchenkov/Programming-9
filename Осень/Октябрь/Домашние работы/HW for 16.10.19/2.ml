let l = ["a";"b";"c";"d";"e"];;

let rec freq = [1;2;4;8;1];;

let rec sum l = 
	match l with 
 [] -> 0
|a :: b -> a + (sum b);;

let rec varieties = List.map (fun x -> (float_of_int x) /. (float_of_int (sum freq))) freq;;

let rec shennon l = 
	match l with
 [] -> 0.
|a :: b -> (-. (a *. (log a) /. (log 2.))) +. (shennon b);;

(shennon varieties);;