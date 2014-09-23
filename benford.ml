(* check Benford's Law *)

open Big_int

let fib_iter cnt callback =
	let a = ref (big_int_of_int 1) and 
		b = ref (big_int_of_int 1) and 
		c = ref (big_int_of_int 0) in
	for i = 0 to (cnt - 1) do 
		if i < 2 then callback !a
		else (c := !a; a := add_big_int !a !b; b := !c; callback !a);
	done;
;;

let calc_freq arr n = 
	let fst = int_of_char (String.get (string_of_big_int n) 0) - int_of_char '0' in 
	arr.(fst) <- arr.(fst) + 1
;;	

let print_freqs arr cnt = 
	Array.iteri (fun i v -> Printf.printf "%i - %0.3f\n" i 
				((float_of_int v) *. 100.0 /. (float_of_int cnt))) arr
;;				
	
let () = (
	let arr = Array.make 10 0 in
	Printf.printf "Enter max fibonacci number count [1...] : %!";
	let fib_cnt = try Some (int_of_string (input_line stdin)) with Failure "int_of_string" -> None in
	match fib_cnt with 
		| Some cnt when cnt > 0 ->	(fib_iter cnt (calc_freq arr); print_freqs arr cnt)
		| None | Some _ -> print_endline "Enter positive integer number, please..."
	)
;;