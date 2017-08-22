print_string "registering square function...\n" ;;
flush stdout ;;

let def f x =
    Printf.printf "square call: %d\n" x; flush stdout;
    reply x * x to f in
Ns.register "square" f vartype
;;

print_string "registering location...\n" ;;
flush stdout ;;

let loc here do {} ;;
Ns.register "squareloc" here vartype ;;

print_string "Done. Waiting...\n" ;;
flush stdout ;;

Join.server() ;;

