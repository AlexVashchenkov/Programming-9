let church_to_int n = (n ((+)1) 0);;
let print_church n = print_int (church_to_int n);;

let two f x = f (f x);;
let zero f x = x;;

let rec int_to_church n f x = if n = 0 then x else (f (int_to_church (n - 1) f x));;

let rec isZero n = if (church_to_int n) = 0 then true else false;;

let rec lambda_or a b = if (isZero a) && (isZero b) then false else true;;

let rec lambda_xor a b = if (isZero a) = true && (isZero b) = true then false else
			 if (isZero a) = true then true else
			 if (isZero b) = true then true else false;;
