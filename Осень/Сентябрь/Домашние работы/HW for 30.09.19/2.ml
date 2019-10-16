let file = open_in "in2.txt";;

open String;;

let rec zip_line s n x = 
	if n = (length s) then [x] else 
	if s.[n] =  ' ' then x :: (zip_line s (n+1) "") else (zip_line s (n+1) (x ^ (make 1 s.[n])));;

let rec find_all l x =
	match l with
 [] -> 0
|a :: b -> if a = x then 1 + (find_all b x) else (find_all b x);; 
			
let rec find_max l l2 x = 
	match l with
 [] -> x
|a :: b -> if (find_all l2 a) > (find_all l2 x) then (find_max b l2 a) else (find_max b l2 x);;

let rec whole_text() = 
	try
		let u = input_line file in u ^ whole_text() 
	with 
		End_of_file -> "";;


let message = (zip_line (whole_text()) 0 "");;

let rec upd l x = 
	match l with
 [] -> [(x,1)]
|(a,b) :: q -> if a = x then (a,b+1) :: q else (a,b) :: (upd q x);;

let rec fill_list s a = 
	 match s with
 [] -> a
|hd :: tl -> (fill_list tl (upd a hd));;

let a = fill_list message [];;

(*let rec make_array a = 
	try
		let u = input_line file in (make_array (fill_list (zip_line u 0 "") a)) 
	with
		End_of_file -> a;;*)
 
let oc = open_out "out2.txt";;
  
let print() =
  (* Write message to file *)
  (* create or truncate file, return channel *)
  (*Printf.fprintf oc "\n\n%s %d\n" (find_max message message "") (find_all message (find_max message message ""));;    write something *)
(List.iter (fun (x,y) -> Printf.fprintf oc "%s %d\n" x y) a);;

print();;
close_out oc;;
