let f = open_in "in_3.txt";;

open String;;

let l = [];;


let rec zip_line s n x = 
	if n = (length s) then [x] else 
	if s.[n] =  ' ' then x :: (zip_line s (n+1) "") else (zip_line s (n+1) (x ^ (make 1 s.[n])));;


let rec find_max l x = 
	match l with
 [] -> x
|a :: b -> if (length a) > (length x) then (find_max b a) else (find_max b x);;

let rec main l =	
	try
		let u = input_line f in ((find_max (zip_line u 0 "") "") :: main())
	with 
		End_of_file -> [];;


let file = "out_3.txt";;

let message = (find_max (main()) "");;

List.iter (fun x -> print_string (x ^ " ")) (main());;
  
let print() =
  (* Write message to file *)
  let oc = open_out file in    (* create or truncate file, return channel *)
  Printf.fprintf oc "%s\n" message;   (* write something *)   
  close_out oc;; 

print();;
	  
