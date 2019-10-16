open String;;

open List;;

let n = read_int();;

let n = 6;;

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

let rec binary_str s n = 
	if n = (String.length s) then [] else
	if s.[n] = '(' then 1 :: (binary_str s (n+1)) else (-1) :: (binary_str s (n+1));;

let rec sum l n = 
	if n >= (length l) then failwith"" else match l with
 [] -> 0
|a :: b -> if n = 0 then a else a + (sum b (n-1));;

let rec piece_sums s n = 
	if n = (String.length s) then [] else (sum (binary_str s 0) n) :: (piece_sums s (n+1));;

let rec number l a = 
	match l with
 [] -> failwith""
|x :: b -> if a = x then 0 else 1 + (number b a);;

let rec concat l = 
	match l with
 [] -> ""
|a :: b -> a ^ (concat b);;

let rec zip d = 
	match d with
 [] -> []
|a :: b -> (concat a) :: (zip b);;

let rec get_elem l n = 
	match l with
 [] -> failwith""
|a :: b -> if n = 0 then a else (get_elem b (n-1));;

let rec check_last l x = 
	match l with
 [] -> (length x) 
|a :: b -> if a < 0 then (number x a) else (check_last b x);;	

let a = Array.init (int_of_float (2. ** (float_of_int n))) (fun x -> (check_last (piece_sums (get_elem (zip d) x) 0) (piece_sums (get_elem (zip d) x) 0)));;

let probability = 1. /. (2. ** (float_of_int n));;

let rec main a pr n = 
	if n = (Array.length a) then 0. else
	(float_of_int a.(n)) *. pr +. (main a pr (n+1));;

main a probability 0;;