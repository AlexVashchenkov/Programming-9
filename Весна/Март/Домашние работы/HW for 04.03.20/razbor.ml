type l = Var of string | App of l * l | Abs of string * l;;

let rec parse_var str i = 
		let (expr,m0) = parse_s str i in 
			if str.[m0] = '-' then 
				let (expr2, m2) = parse_x str (m0 + 1) in 
					(Binop (expr,Minus,expr2),m2) 
			else 
				(expr,m0) 

    and parse_lambda str i= 
         	let (expr,m) = parse_var str i in 
			if str.[m] = '+' then 
				let (expr2, m2) = parse_s str (m + 1) in 
					(Binop (expr,Plus,expr2),m2) 
			else 
				(expr,m);; 
	