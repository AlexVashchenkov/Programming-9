(*Матожидание глубины для правильных скобочных записей длины 2n*)

open List;;

let n = 8;;

let rec find l x = 
	match l with
 [] -> 0
|a :: b -> if a = x then 1 + (find b x) else (find b x);;

let rec sep l = 
	match l with
 [] -> true
|a :: b -> if (find l a) > 1 then false else (sep b);;

let rec sep_m l = 
	match l with
 [] -> []
|[] :: q -> (sep_m q)
|(a :: b) :: q -> if (sep (a :: b)) = false then (sep_m q) else (a :: b) :: (sep_m q);;


let rec input_list n = if n = 0 then [] else (read_int()) :: (input_list (n-1));;

let rev l = List.rev l;;

let draw_m m x = List.map (fun a -> m::a) x ;;

let rec alpha k l =
	match l with
| [] -> []
| a :: b -> (draw_m a k) @ (alpha k b);;

let rec numbers n l =
	if n = 0 then [[]] else
let k = numbers (n-1) l in (alpha k l);;

let d = numbers n ["(";")"];; 

let rec delete_more d = 
	match d with
 [] -> []
|[] :: q -> (delete_more q)
|(a :: b) :: q -> if (sep (a :: b)) = false then (delete_more q) else (a :: b) :: (delete_more q);;

(*List.iter (fun x -> (List.iter (fun s -> print_string s) x); print_string " ") d;;*)

let rec binary_str s n = 
	if n = (String.length s) then [] else
	if s.[n] = '(' then 1 :: (binary_str s (n+1)) else (-1) :: (binary_str s (n+1));;

let rec sum l n = 
	if n >= (length l) then failwith"" else match l with
 [] -> 0
|a :: b -> if n = 0 then a else a + (sum b (n-1));;

let rec piece_sums s n = 
	if n = (String.length s) then [] else (sum (binary_str s 0) n) :: (piece_sums s (n+1));;

let rec positive l = 
	match l with
 [] -> true 
|a :: b -> if a < 0 then false else (positive b);;	

let rec get_elem l n = 
	match l with
 [] -> failwith"" 
|a :: b -> if n = 0 then a else (get_elem b (n-1));;	

let rec right_string s = 
if (positive (piece_sums s 0)) = true && (get_elem (piece_sums s 0) (length (piece_sums s 0) - 1)) = 0 then true else false;;

let rec drop_out d = 
	match d with
 [] -> []
|a :: b -> if (right_string a) then a :: (drop_out b) else (drop_out b);;

let rec concat l = 
	match l with
 [] -> ""
|a :: b -> a ^ (concat b);;

let rec zip d = 
	match d with
 [] -> []
|a :: b -> (concat a) :: (zip b);;

(*List.iter (fun x -> print_string x;print_string "\n") (drop_out (zip d));;*)

let rec depth_list str index n = 
	if index = (String.length str) then [] else (if str.[index] = '(' then n :: (depth_list str (index+1) (n+1)) else n :: (depth_list str (index+1) (n-1)));;

let rec max l = 
	let rec maximum l x = 
		match l with
 	 [] -> x
	|a :: b -> if a > x then (maximum b a) else (maximum b x)
in (maximum l (hd l));;

let depth str = max (depth_list str 0 0);;

let probability = 1. /. (float_of_int (length (drop_out (zip d))));;

let all_depths = Array.init (List.length (zip d)) (fun x -> (depth (get_elem (drop_out (zip d)) x)));;

let rec main all_depths n = 
	if n = (Array.length all_depths) then 0. else (float_of_int all_depths.(n)) *. probability +. (main all_depths (n+1));;

main all_depths n;; 