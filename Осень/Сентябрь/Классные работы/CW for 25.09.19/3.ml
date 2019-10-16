let f = open_in_bin "x.txt";;

let l = [];;

let message = "\n";;

let print() =
  (* Write message to file *)
  let oc = open_out "x.txt" in    (* create or truncate file, return channel *)
  fprintf oc "%s\n" message;   (* write something *)   
  close_out oc;; 

let rec v_read_file() = 

	try
		let u = input_byte f in 
