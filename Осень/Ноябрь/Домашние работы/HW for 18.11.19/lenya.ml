open String;;
let rec dva x = if x < 0 then failwith "" else if x <= 1 then string_of_int x else (dva (x / 2)) ^ (string_of_int (x mod 2));;

print_string (dva 256);;
print_string " ";;

let rec equal y x = let k = dva x in if (length k) < y then failwith "" else (make ((length k) - y) '0') ^ k;;

print_string (equal 9 256);;
print_string " ";;

let rec serial l y x n = let s = (equal y x) in (if (n + 1) = length s then s.[n] :: l else serial (s.[n] :: l) y x (n + 1));;

List.iter (fun x -> print_int x) (serial ['1';'0';'1';'1';'1';'0'] 10 256 0);;
