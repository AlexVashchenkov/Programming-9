let church_to_int n = (n ((+)1) 0);;
let print_church n = print_int (church_to_int n);;

let rec int_to_church n f x = if n = 0 then x else (f (int_to_church (n - 1) f x));;

let rec isZero n = if (church_to_int n) = 0 then true else false;;

(isZero two);;
             