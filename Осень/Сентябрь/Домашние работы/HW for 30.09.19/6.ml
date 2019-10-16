open String;;

let rec modify_str s n = 
	if n = (length s) then "" else
	if s.[n] = 'a' || s.[n] = 'e' || s.[n] = 'i' || s.[n] = 'o' || s.[n] = 'u' 
	then (sub s 0 (n)) ^ ((make 1 s.[n]) ^ "0x301") ^ (sub s (n+1) ((length s) - n - 1))
	else (make 1 s.[n]) ^ (modify_str s (n+1));;

print_string (modify_str 

 