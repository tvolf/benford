(* check Benford's Law *)

open Big_int

let rec fib = 
	let memo = Hashtbl.create 1 in
	let get h k = try Some (Hashtbl.find h k) with Not_found -> None in
	fun n ->
		match n with 
		| 0 | 1 -> big_int_of_int 1
		| n -> match get memo n with 
			 | Some x -> x
			 | None -> let r = add_big_int (fib (n-1)) (fib (n-2)) in
					   ( Hashtbl.add memo n r; r)
;;			


let rec calc_freq fib_cnt = 
    let memo = Array.make 10 0 in 
   (
		for i = 0 to (fib_cnt - 1) do 
			let f = fib i in 
			let fst = int_of_char (String.get (string_of_big_int f) 0) - int_of_char '0' in 
			memo.(fst) <- memo.(fst) + 1
		done;
		Array.iteri (fun i v -> Printf.printf "%i - %0.3f\n" i 
					((float_of_int v) *. 100.0 /. (float_of_int fib_cnt))) memo
	)
;;
			
let () = (
	Printf.printf "Enter max fibonacci number count [1...] : %!";
	let err_msg = "Enter positive integer number, please..." in
	let fib_cnt = try Some (int_of_string (input_line stdin)) with Failure "int_of_string" -> None in
	match fib_cnt with 
		| Some n when n > 0 ->	calc_freq n
		| None | Some _ -> print_endline err_msg
	)
;;