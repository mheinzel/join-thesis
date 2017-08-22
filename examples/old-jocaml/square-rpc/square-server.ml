let def f x =
    Printf.printf "received: %d\n" x;
    flush stdout;
    reply x * x to f in
Ns.register "square" f vartype
;;

print_string "registered square function! Waiting...\n" ;;
flush stdout ;;

Join.server() ;;

