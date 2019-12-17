let rec is_prefix = 
	let mixes (a,b) = 
		match (a,b) with 

	 (a1::al,b1::bl) -> if a1 = b1 then (mixes (al,bl)) else true
	|(_,_) -> false
	in 									
