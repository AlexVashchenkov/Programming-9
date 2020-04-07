let print_church n = print_int (n ((+)1) 0);;
let two f x = f (f x);;
let four f x = f (f (f (f x)));;
