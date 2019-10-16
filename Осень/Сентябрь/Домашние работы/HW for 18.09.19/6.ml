(*По списку опозданий вычислить вероятность, что человек опоздает*)

open List;;

let l = [-1;-2;-3;-4;-5;-6;-7;-8;-9;-10];; 

let rec recorgnize l x y = 
	match l with
 [] -> (x,y)
|a :: b -> if a > 0 then (recorgnize b (x+1) y) else (recorgnize b x (y+1));;

Printf.printf "The probability to be late is: %f" ((float_of_int (fst (recorgnize l 0 0))) /. (float_of_int (length l)));;