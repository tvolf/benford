(* check Benford's Law *)

open Big_int

let make_fib_stream () = 
	let a = ref (big_int_of_int 0) and 
		b = ref (big_int_of_int 1) in
	Stream.from 
	(fun _ -> 
		let sum = add_big_int !a !b in   
		(b := !a; 
 		 a := sum; 
		 Some !a);
	)
;;

let stream_take n stream_in =
    let cnt = ref n in 
	fun () -> 	
		Stream.from 
		(fun _ -> 	
			if !cnt <= 0 then None 
			else (cnt := !cnt - 1; Some (Stream.next stream_in))
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
	let fib_stream_all = make_fib_stream () in 
	let fib_cnt = try 
					Some (int_of_string (input_line stdin)) 
				  with 
					Failure "int_of_string" -> None in
	match fib_cnt with 
		| Some cnt when cnt > 0 ->	
			let stream = stream_take cnt fib_stream_all () in 
			(fill_arr arr stream;
			 print_freqs arr cnt)
		| None | Some _ -> print_endline "Enter positive integer number, please..."
	)
;;