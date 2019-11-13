type tree = Node of tree * tree | Leaf of char;;

let l = [(Leaf 'x',10);(Leaf 'y',12);(Leaf 'z',4)];;

let sum = List.fold_left (fun x y -> x + y) 0 List.map (fun x -> (snd x)) l;;

let rec give_freqs = List.map (fun x -> 