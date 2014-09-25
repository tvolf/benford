(* check Benford's Law *)

open Big_int

let make_fib_stream () = 
	let a = ref (big_int_of_int 0) and 
		b = ref (big_int_of_int 1) and 
		c = ref (big_int_of_int 0) and 
		cnt = ref 0 in
	fun max_cnt ->
		Stream.from 
		(fun _ -> 
			if !cnt = max_cnt then None 
			else (c := !a; 
				  a := add_big_int !a !b; 
				  b := !c; 
				  cnt := !cnt + 1;
				  Some !a);
		)
;;


let add_one_fib arr n = 
	let fst = int_of_char (String.get (string_of_big_int n) 0) - int_of_char '0' in 
	arr.(fst) <- arr.(fst) + 1
;;	

let fill_arr arr stream = 
	Stream.iter (fun fib -> add_one_fib arr fib) stream
;;	

let print_freqs arr cnt = 
	Array.iteri (fun i v -> Printf.printf "%i - %0.3f\n" i 
				((float_of_int v) *. 100.0 /. (float_of_int cnt))) arr
;;				
	
let () = (
	let arr = Array.make 10 0 in
	Printf.printf "Enter max fibonacci number count [1...] : %!";
	let fib_stream = make_fib_stream () in 
	let fib_cnt = try 
					Some (int_of_string (input_line stdin)) 
				  with Failure "int_of_string" -> None in
	match fib_cnt with 
		| Some cnt when cnt > 0 ->	(fill_arr arr (fib_stream cnt); print_freqs arr cnt)
		| None | Some _ -> print_endline "Enter positive integer number, please..."
	)
;;