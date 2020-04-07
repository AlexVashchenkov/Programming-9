let rec print_church n = print_int (n (fun x -> x + 1) 0);;

let two f x = f (f x);;
let three f x = f (f (f x));;


print_church (two two two);;

let rec inc n = fun f x -> f (n f x);;
