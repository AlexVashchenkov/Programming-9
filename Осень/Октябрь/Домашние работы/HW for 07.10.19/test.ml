(*код Сани*)

let l = read_line();;
let a = String.length a;;

let rec razbor str poz pozl count r otvet = 
	if count = 0 then otvet else
	if String.get str poz = '.' then (razbor str (poz*256) 1 (count + 1) 0 (otvet + (r * poz))) else
	(razbor str (poz + 1) (pozl * 10) count ( (otvet + (r * poz)))