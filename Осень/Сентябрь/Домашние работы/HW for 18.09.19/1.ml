(*Матожидание старшей цифры чисел от 1 до 999*)

open Array;;

let a = init 999 (fun x -> x+1);;

let probability = 1. /. 1000.;;

let a_new = Array.init 999 (fun n -> if a.(n) < 10 then a.(n) else 
				(if a.(n) < 100 then (a.(n) / 10) else (a.(n) / 100)));;
let rec sum a_new n =
	if n = (length a_new) then 0. else
	(float_of_int a_new.(n)) *. probability +. (sum a_new (n+1));;

Array.iter (fun x -> print_int x; print_string " ") a_new;;