(*Найти вероятность того, что десятичное n-значное число можно прочитать как двоичное*)

open String;;

let n = read_int();;

let rec probability = if n = 1 then (1. /. 10.) else (1. /. (9. *. (10. ** (float_of_int (n - 1)))));;

let all_numbers = 
	if n = 1 then 2. else (2. ** (float_of_int (n - 1)));;

print_float (all_numbers *. probability);;