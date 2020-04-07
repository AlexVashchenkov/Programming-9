open Array;;

let x = [|1;4;9;16|];;
let y = [|2;4;6;8;10;12;14;16|];;
let m = (length x);;
let n = (length y);;

let lcs = Array.init m (fun x -> if x = 0 then Array.init n (fun y -> 0) else (Array.init n (fun y -> if y = 0 then 0 else 1)));;
let prev = Array.init m (fun x -> if x = 0 then Array.init n (fun y -> (0,0)) else (Array.init n (fun y -> if y = 0 then (0,0) else (1,1))));;


let rec fill_matr lcs prev i j = 
	if i >= m then prev else
	if j >= n then (fill_matr lcs prev (i+1) 1) else
	if x.(i) = y.(j) then (fill_matr (lcs.(i).(j) <- (lcs.(i-1).(j-1) + 1);lcs) (prev.(i).(j) <- (i-1,j-1) ;prev) i (j+1)) else
	if lcs.(i-1).(j) >= lcs.(i).(j-1) then (fill_matr (lcs.(i).(j) <- (lcs.(i-1).(j));lcs) (prev.(i).(j) <- (i-1, j) ;prev) i (j+1)) else
			      (fill_matr ((lcs.(i).(j) <- (lcs.(i).(j-1)));lcs) (prev.(i).(j) <- (i, j-1);prev) i (j+1));;


let rec print_lcs prev i j = 
	if i = 0 || j = 0 then () else
	if prev.(i).(j) = (i-1,j-1) then ((print_lcs prev (i-1) (j-1));print_int x.(i);print_string " ") else
	if prev.(i).(j) = (i-1,j) then (print_lcs prev (i-1) j) else (print_lcs prev i (j-1));;

print_lcs (fill_matr lcs prev 1 1) (m-1) (n-1);;