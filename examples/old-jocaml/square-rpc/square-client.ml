print_string "about to lookup square function...\n" ;;
flush stdout ;;

let square : int -> int = Ns.lookup "square" vartype ;;

print_string "success!\n\n" ;;
flush stdout ;;

let rec request () =
	print_string "number to square: ";
	flush stdout ;
	let n = int_of_string (input_line stdin) in
	let result = square n in
	Printf.printf "got a result: %d\n" result ;
	flush stdout ;
	request ()
;;

request () ;;
