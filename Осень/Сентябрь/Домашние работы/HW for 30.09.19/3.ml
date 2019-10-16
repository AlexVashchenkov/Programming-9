let f = open_in "in3.txt";;

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
		let u = input_line f in u ^ whole_text() 
	with 
		End_of_file -> "";;

let message = (zip_line (whole_text()) 0 "");;

let rec unique_words l s = 
	match l with
 [] -> s
|a :: b -> if (find_all s a) = 0 then (unique_words b (a :: s)) else (unique_words b s);;
 
let rec list_of_words = List.map (fun x -> (x,(find_all message x))) (unique_words message []);;

let rec sort lst =
   match lst with
     [] -> []
   | head :: tail -> insert head (sort tail)
 and insert elt lst =
   match lst with
     [] -> [elt]
   | head :: tail -> if (snd elt) >= (snd head) then elt :: lst else head :: insert elt tail;;
 
(*sort [("a",0);("b",6);("c",4);("d",0)];;*)

let oc = open_out "out3.txt";;

let rec cut l n = 
	match l with
 [] -> [] 
|a :: b -> if (length (fst a)) <= 3 then (cut b n) else 
	   if n = 0 then [] else a :: (cut b (n-1));;

let print() =
  (* Write message to file *)
  (* create or truncate file, return channel *)
(*(List.iter (fun x -> Printf.fprintf oc "%s %d\n" x (find_all message x)) message)*)
(List.iter (fun (x,y) -> Printf.fprintf oc "%s %d\n" x y) (cut (sort (list_of_words)) 50));;   (* write something *)

print();;
close_out oc;;
