open String;;

let rec generate n k = 
	if n > k then "" else (make 1 (Char.chr (n+192))) ^ (generate (n+1) k);;

open Printf
  
let file = "6.txt"
let message = (generate 0 63);;
  
let print() =
  (* Write message to file *)
  let oc = open_out file in    (* create or truncate file, return channel *)
  fprintf oc "%s\n" message;   (* write something *)   
  close_out oc;; 

print();; 