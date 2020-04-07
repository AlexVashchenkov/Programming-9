let print_church n = print_int (n ((+)1) 0);;

let rec int_to_church n f x = if n = 0 then x else (f (int_to_church (n - 1) f x));;

print_church (int_to_church 7);;
