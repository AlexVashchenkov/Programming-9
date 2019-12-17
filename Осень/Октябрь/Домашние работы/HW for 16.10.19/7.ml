type prefix = Question of prefix * prefix | Value of int;;

let tree = (Question (Value 5,Question (Value 7,Value 21)));;

let answer = 5;;

let rec find_answers t c n = 
	match t with
 Value x -> if x = c then n else []
|Question (l,r) -> (find_answers l c (n @ [0])) @ (find_answers r c (n @ [1]));; 
	
