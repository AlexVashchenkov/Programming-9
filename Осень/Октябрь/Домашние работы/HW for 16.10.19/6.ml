type prefix = Question of prefix * prefix | Value of int;;

let tree = (Question (Value 5,Question (Value 7,Value 21)));;

let answers = [0];;

let rec find_answer lst t = 
	match (t,lst) with
 (Value x,[]) -> x
|(Value x,a :: b) -> failwith""
|(Question (l,r),[]) -> failwith""
|(Question (l,r),a :: b) -> if a = 0 then (find_answer b l) else (find_answer b r);;
